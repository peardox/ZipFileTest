{
  Copyright 2019-2021 Michalis Kamburelis.

  Modified by Peardox

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic. }
// {$define verboseLogging}
// {$define deleteWhileTesting}

unit ZipUrls;

interface

uses SysUtils, Classes, Zipper,
  CastleLog, CastleUtils, URIParser,
  CastleFilesUtils, CastleDownload,
  CastleStringUtils, CastleURIUtils;

type
  EZipError = class(Exception);

  TZipFileSystem = class(TComponent)
  private
    fProtocol: String;                  // Name of protocol - zip-data-NN
    fZipFile: String;                   // Zip filename if created as file
    fZipStream: TMemoryStream;          // Zip stream if created as stream
    fZipTransientStream: TMemoryStream; // Zip stream copy for InputStream
    fUseStream: Boolean;                // Is this Zip a stream or file?
    fFriendlyName: String;              // A more friendly name (extension removed)
    fUnzip: TUnZipper;                  // The UnZip component
    fZipFiles: TStringList;             // Files in Zip also used to hold output stream
    fRawFiles: TStringList;             // Files in Zip also used to hold output stream
    procedure DoStartZipFile(Sender: TObject; const AFile: string);
    procedure DoEndZipFile(Sender: TObject; const Ratio: Double);
    procedure DoDoneOutZipStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoCreateOutZipStream(Sender: TObject; var AStream: TStream; AItem: TFullZipFileEntry);
    procedure DoOpenInputStream(Sender: TObject; var AStream: TStream);
    procedure DoCloseInputStream(Sender: TObject; var AStream: TStream);
    procedure CommonSetup;
    procedure SetZipFile(const AUrl: String);
    procedure SetZipStream(const AStream: TStream; const AUrl: String = '');
  public
    function GetStream(const AUrl: string): TStream;
    function GetStream(const AUrl: string; out MimeType: string): TStream;
    function ReadZip(const AUrl: string; out MimeType: string): TStream;
    function getProtocol: String;
    function RePackZipFile(const AZipFilename: String): Boolean;
    constructor Create(AOwner: TComponent); override;
    constructor Create(AOwner: TComponent; const AUrl: string);
    constructor Create(AOwner: TComponent; const AStream: TStream; const AUrl: string = '');
    destructor Destroy; override;
    property ZipFile: String read fZipFile;
    property Protocol: String read getProtocol;
    property FriendlyName: String read fFriendlyName write fFriendlyName;
    property Files: TStringList read fZipFiles;
    property RawFiles: TStringList read fRawFiles;
  end;

function ReWriteZipFile(const AZipFileIn: String;const AZipFileOut: String): Boolean;

implementation

{ Utility Functions ------------------------------------------------------- }

function ReWriteZipFile(const AZipFileIn: String;const AZipFileOut: String): Boolean;
var
  InFileStream: TStream;
  CopyFileStream: TZipFileSystem;
begin
  InFileStream := Download(URIToFilenameSafe(AZipFileIn));
  CopyFileStream := TZipFileSystem.Create(nil, InFileStream, AZipFileIn);
  FreeAndNil(InFileStream);

  Result := CopyFileStream.RePackZipFile(URIToFilenameSafe(AZipFileOut));
  FreeAndNil(CopyFileStream);
end;

{ TZipFileSystem ---------------------------------------------------------- }

function TZipFileSystem.getProtocol: String;
begin
  if not(fProtocol = EmptyStr) then
    Result := fProtocol + ':'
  else
    begin
      Result := fProtocol;
      raise EZipError.CreateFmt('EZipError : Using undefined protocol for %s', [fZipFile]);
    end;
end;

procedure TZipFileSystem.CommonSetup;
var
  I: Integer;
begin
  if not(fZipFiles = nil) then
    FreeAndNil(fZipFiles);
  if not(fRawFiles = nil) then
    FreeAndNil(fRawFiles);
  fZipFiles := TStringList.Create;
  fZipFiles.OwnsObjects := True;
  fRawFiles := TStringList.Create;
  fRawFiles.OwnsObjects := True;
  fZipFiles.Sorted := True;
  fZipFiles.Duplicates := dupError;
  fUnzip.Examine;
  for I := 0 to fUnzip.Entries.Count - 1 do
    begin
      fZipFiles.AddObject(fUnzip.Entries[I].ArchiveFileName, nil);
      fRawFiles.AddObject(fUnzip.Entries[I].ArchiveFileName, fUnzip.Entries[I] as TZipFileEntry);
    end;
end;

procedure TZipFileSystem.SetZipStream(const AStream: TStream; const AUrl: String = '');
var
  I: Integer;
  P: String;
  F: String;
  E: SizeInt;
begin
  if not(AStream = nil) then
    begin
      if fZipFile = EmptyStr then
        begin
          fZipFile :=  'UnNamed-Zip-File';
          F := fZipFile;
        end
      else
        begin
          fZipFile := URIToFilenameSafe(AUrl);
          F := LowerCase(ExtractFileName(fZipFile));
        end;

      fUseStream := True;
      fZipStream := TMemoryStream.Create;
      fZipStream.LoadFromStream(AStream);

      // Create a friendly name for the zip
      E := F.IndexOf('.zip');
      if E = 0 then // A file called .zip is being opened?
        F := 'dotzip'
      else if E > 0 then
        F := F.Remove(E);
      fFriendlyName := F;

      // Create a protocol for the zip
      I := 1;
      repeat
        P := 'zip-data-' + Format('%d', [I]);
        if URIValidProtocol(P) then
          if not(RegisteredUrlProtocol(P)) then
            Break;
        Inc(I);
      until false;

      fProtocol := P;
      RegisterUrlProtocol(fProtocol, @ReadZip, nil);
      WriteLnLog('Registered Protocol ' + fProtocol + ' for ' + fZipFile);

      CommonSetup;
    end
  else
    raise EZipError.Create('Attempt to open nil Zip Stream');
end;

procedure TZipFileSystem.SetZipFile(const AUrl: String);
var
  I: Integer;
  P: String;
  F: String;
  E: SizeInt;
begin
  if not(AUrl = '') then
    begin
      fZipFile := URIToFilenameSafe(AUrl);

      if fZipFile = EmptyStr then
        raise EZipError.Create('Attempt to open an unnamed Zip File');

      // Create a friendly name for the zip
      F := LowerCase(ExtractFileName(fZipFile));
      E := F.IndexOf('.zip');
      if E = 0 then // A file called .zip is being opened?
        F := 'dotzip'
      else if E > 0 then
        F := F.Remove(E);
      fFriendlyName := F;

      fUseStream := False;

      // Create a protocol for the zip
      I := 1;
      repeat
        P := 'zip-data-' + Format('%d', [I]);
        if URIValidProtocol(P) then
          if not(RegisteredUrlProtocol(P)) then
            Break;
        Inc(I);
      until false;

      fProtocol := P;
      RegisterUrlProtocol(fProtocol, @ReadZip, nil);
      WriteLnLog('Registered Protocol ' + fProtocol + ' for ' + fZipFile);

      fUnzip.FileName := fZipFile;
      CommonSetup;
    end
  else
    raise EZipError.Create('Attempt to open Zip File with no filename');
end;

procedure TZipFileSystem.DoStartZipFile(Sender: TObject; const AFile: string);
begin
  {$ifdef verboseLogging}
  WritelnLog('UnZip : DoStartZipFile : AFile = ' + AFile);
  {$endif}
end;

procedure TZipFileSystem.DoEndZipFile(Sender: TObject; const Ratio: Double);
begin
  {$ifdef verboseLogging}
  WritelnLog('UnZip : DoEndZipFile : Ratio = ' + FloatToStr(Ratio));
  {$endif}
end;

procedure TZipFileSystem.DoCreateOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
var
  I: Integer;
begin
  {$ifdef verboseLogging}
  WritelnLog('UnZip : DoCreateOutZipStream : ' + AItem.ArchiveFileName);
  {$endif}
  if fZipFiles.Find(AItem.ArchiveFileName, I) then
    begin
      AStream := TMemorystream.Create;
      fZipFiles.Objects[I] := AStream;
    end
  else
    begin
      raise EZipError.CreateFmt('Can''t locate %s in %s', [
        AItem.ArchiveFileName,
        fZipFile
      ]);
    end;
end;

procedure TZipFileSystem.DoDoneOutZipStream(Sender: TObject; var AStream: TStream;
  AItem: TFullZipFileEntry);
begin
  {$ifdef verboseLogging}
  WritelnLog('UnZip : DoDoneOutZipStream : ' + AItem.ArchiveFileName);
  {$endif}
  if not(AStream.Position = 0) then
    AStream.Position:=0;
end;

procedure TZipFileSystem.DoOpenInputStream(Sender: TObject; var AStream: TStream);
begin
  {$ifdef verboseLogging}
  if AStream = nil then
    WriteLnLog('DoOpenInputStream : AStream = nil')
  else
    WriteLnLog('DoOpenInputStream : AStream = ' + AStream.ClassName);
  {$endif}
  if fUseStream then // fUseStream = was created from stream not file
    begin  // fZipStream is a TMemoryStream
      if not(Assigned(AStream)) and Assigned(fZipStream) then
        begin
          fZipTransientStream := TMemoryStream.Create;
          fZipTransientStream.LoadFromStream(fZipStream);
          AStream := fZipTransientStream;
        end;
    end;
end;

procedure TZipFileSystem.DoCloseInputStream(Sender: TObject; var AStream: TStream);
begin
  {$ifdef verboseLogging}
  if AStream = nil then
    WriteLnLog('DoCloseInputStream : AStream = nil')
  else
    WriteLnLog('DoCloseInputStream : AStream = ' + AStream.ClassName);
  {$endif}
end;

function TZipFileSystem.GetStream(const AUrl: string): TStream;
var
  MimeType: String;
begin
  Result := ReadZip(AUrl, MimeType);
end;

function TZipFileSystem.GetStream(const AUrl: string; out MimeType: string): TStream;
begin
  Result := ReadZip(AUrl, MimeType);
end;

function TZipFileSystem.ReadZip(const AUrl: string; out MimeType: string): TStream;
var
  I: Integer;
  FileInZip: String;
  U: TURI;
begin
  Result := nil;

  {$ifdef verboseLogging}
  WriteLnLog('ReadZip ' + AUrl);
  {$endif}

  U := ParseURI(AUrl);
  FileInZip := PrefixRemove('/', U.Path + U.Document, false);
  
  if fZipFiles.Find(FileInZip, I) then
    begin
      { If the requested file hasn't been extracted yet then do so }
      if fZipFiles.Objects[I] = nil then
        begin
          fUnzip.UnZipFile(FileInZip);
        end;

      { We now have an Stream object - best double check anyway...}
      if not(fZipFiles.Objects[I] = nil) then
        begin
          MimeType := URIMimeType(FileInZip);
          {$ifdef verboseLogging}
          WriteLnLog('Returning stream');
          {$endif}
          Result := fZipFiles.Objects[I] as TMemoryStream;
          fZipFiles.Objects[I] := nil;        
         end
       else
         raise EZipError.Create('fZipFiles.Objects[I] = nil');
    end
  else
    begin
      raise EZipError.CreateFmt('Can''t locate %s in %s', [
        FileInZip,
        fZipFile
      ]);
    end;
end;

constructor TZipFileSystem.Create(AOwner: TComponent; const AStream: TStream; const AUrl: string = '');
begin
  Create(AOwner);
  SetZipStream(AStream, AUrl);
end;

constructor TZipFileSystem.Create(AOwner: TComponent; const AUrl: string);
begin
  Create(AOwner);
  SetZipFile(AUrl);
end;

constructor TZipFileSystem.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  fUnzip := TUnZipper.Create;
  fUnzip.OnCreateStream := @DoCreateOutZipStream;
  fUnzip.OnDoneStream := @DoDoneOutZipStream;
  fUnzip.OnStartFile := @DoStartZipFile;
  fUnzip.OnEndFile := @DoEndZipFile;
  fUnZip.OnOpenInputStream := @DoOpenInputStream;
  fUnZip.OnCloseInputStream := @DoCloseInputStream;
end;

destructor TZipFileSystem.Destroy;
begin
  fZipTransientStream := nil;
//  fZipStream := nil;
  FreeAndNil(fZipStream);
  FreeAndNil(fRawFiles);
  FreeAndNil(fZipFiles);
  FreeAndNil(fUnzip);
  WriteLnLog('UnRegistering Protocol ' + fProtocol);
  UnregisterUrlProtocol(fProtocol);
  inherited;
end;

function TZipFileSystem.RePackZipFile(const AZipFilename: String): Boolean;
var
  NewZipper: TZipper;
  i: Integer;
  fe: TZipFileEntry;
  ne: TZipFileEntry;
  s: TStream;
begin
  Result := false;
  if not(fUseStream) then
    raise EZipError.Create('EZipError : RePackZipFile needs to be passed a stream based zip file');
  
  {$ifdef deleteWhileTesting}
  if FileExists(AZipFilename) then
    DeleteFile(AZipFilename);
  {$endif}

  NewZipper := TZipper.Create;
  NewZipper.FileName := AZipFileName;
  try
    for i := 0 to RawFiles.Count -1 do
      begin
        fe := RawFiles.Objects[i] as TZipFileEntry;
        {$ifdef verboseLogging}
        WriteLnLog(IntToStr(i) + ' : Trying to add ' + ExcludeTrailingPathDelimiter(fe.ArchiveFileName) + ' to ' + AZipFilename);
        {$endif}
        ne := NewZipper.Entries.Add as TZipFileEntry;
        if fe.IsDirectory then
          begin
            ne.Stream := TStringStream.Create('');
            ne.ArchiveFileName := ExcludeTrailingPathDelimiter(fe.ArchiveFileName);
            ne.UTF8ArchiveFileName := ExcludeTrailingPathDelimiter(fe.ArchiveFileName);
            if not(fe.DiskFileName = EmptyStr) then
              ne.DiskFileName := ExcludeTrailingPathDelimiter(fe.DiskFileName)
            else
              ne.DiskFileName := ne.ArchiveFileName;
            if not(fe.UTF8DiskFileName = EmptyStr) then
              ne.UTF8DiskFileName := ExcludeTrailingPathDelimiter(fe.UTF8DiskFileName)
            else
              ne.UTF8DiskFileName := ne.DiskFileName;
          end
        else
          begin
            ne.Stream := GetStream(fe.ArchiveFileName);
            ne.ArchiveFileName := fe.ArchiveFileName;
            ne.UTF8ArchiveFileName := fe.UTF8ArchiveFileName;
            ne.DiskFileName := fe.DiskFileName;
            ne.UTF8DiskFileName := fe.UTF8DiskFileName;
          end;
        ne.Size := fe.Size;
        ne.DateTime := fe.DateTime;
        ne.OS := fe.OS;
        ne.Attributes := fe.Attributes;
        ne.CompressionLevel := fe.CompressionLevel;
      end;

    NewZipper.ZipAllFiles;

    Result := true;
  finally
    for i := 0 to NewZipper.Entries.Count -1 do
      begin
        fe := NewZipper.Entries[i] as TZipFileEntry;
        if not(fe.Stream = nil) then
          begin
            s := fe.Stream;
            FreeAndNil(s);
          end;
      end;
    FreeAndNil(NewZipper);
  end;
end;

end.
