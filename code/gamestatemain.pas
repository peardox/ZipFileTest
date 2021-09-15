{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes, Math, CastleDownload, Zipper, ZipUrls, CastleTimeUtils, CastleLog,
  CastleVectors, CastleUIState, CastleComponentSerialize, CastleScene,
  CastleViewport, CastleUIControls, CastleControls, CastleKeysMouse;

type
  { TCastleSceneHelper }
  TCastleSceneHelper = class helper for TCastleScene
  public
    function Normalize: TVector3;
    function IsVisible: Boolean;
  end;

  { TCastleViewportHelper }
  TCastleViewportHelper = class helper for TCastleViewport
  public
    procedure ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
    procedure ViewFromRadius(const ARadius: Single; const AElevation: Single; const ATheta: Single);
  end;

  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    LabelLoaded: TCastleLabel;
    Scene1: TCastleScene;
    Viewport1: TCastleViewport;
    Button1: TCastleButton;
    Button2: TCastleButton;
    Button3: TCastleButton;
    Button4: TCastleButton;
    ZipFile: TZipFileSystem;
    ZipStream: TZipFileSystem;
    procedure DoButton1Click(Sender: TObject);
    procedure DoButton2Click(Sender: TObject);
    procedure DoButton3Click(Sender: TObject);
    procedure DoButton4Click(Sender: TObject);
    procedure LoadModel(AUrl: String);
    procedure DoEndFile(Sender : TObject; Const Ratio : Double);
    procedure DoStartFile(Sender : TObject; Const AFileName : String);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Render; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function  Press(const Event: TInputPressRelease): Boolean; override;
    function  RePackZipFile(const AZipFilename: String; AZipFileSystem: TZipFileSystem): Boolean;
  end;

var
  StateMain: TStateMain;
  AppTime: Int64;
  
implementation

uses SysUtils;

{ TCastleSceneHelper }

{ Normalize fits the model in a 1x1x1 cube and centers it }
function TCastleSceneHelper.Normalize: TVector3;
begin
  Result := TVector3.Zero;
  if not(Self = nil) then
    begin
    if not BoundingBox.IsEmptyOrZero then
      begin
        if BoundingBox.MaxSize > 0 then
          begin
            Result := BoundingBox.Size;
            Center := Vector3(Min(BoundingBox.Data[0].X, BoundingBox.Data[1].X) + (BoundingBox.SizeX / 2),
                              Min(BoundingBox.Data[0].Y, BoundingBox.Data[1].Y) + (BoundingBox.SizeY / 2),
                              Min(BoundingBox.Data[0].Z, BoundingBox.Data[1].Z) + (BoundingBox.SizeZ / 2));
            Scale := Vector3(1 / BoundingBox.MaxSize,
                             1 / BoundingBox.MaxSize,
                             1 / BoundingBox.MaxSize);

            Translation := -Center;
          end;
      end;
    end;
end;

function TCastleSceneHelper.IsVisible: Boolean;
begin
  Result := IsVisibleNow;
end;

{ TCastleViewportHelper }

{ ViewFromRadius sets the camera ARadius from (0,0,0) 
  at AElevation pointing at (0,0,0) from ATheta radians 
  around the Y-Axis}
procedure TCastleViewportHelper.ViewFromRadius(const ARadius: Single; const AElevation: Single; const ATheta: Single);
begin
    ViewFromRadius(ARadius, Vector3(sqrt(ARadius) * Cos(ATheta), AElevation, sqrt(ARadius) * Sin(ATheta)));
end;

{ ViewFromRadius sets the camera at ARadius pointing at (0,0,0)
  in ADirection on the Y-Axis}
procedure TCastleViewportHelper.ViewFromRadius(const ARadius: Single; const ADirection: TVector3);
begin
  Camera.Up := Vector3(0, 1, 0);
  Camera.Direction := -ADirection;
  Camera.Position  := ARadius * ADirection.Normalize;
end;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
var
  TestStream: TStream;
begin
  inherited;
  InitializeLog();
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
  ZipFile := TZipFileSystem.Create(Self, 'castle-data:/Tree.zip');
  TestStream := Download('castle-data:/kira_and_killer_queen.zip');
  ZipStream := TZipFileSystem.Create(Self, TestStream, 'castle-data:/kira_and_killer_queen.zip');
  FreeAndNil(TestStream);
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  LabelLoaded := DesignedComponent('LabelLoaded') as TCastleLabel;
  Scene1 :=  DesignedComponent('Scene1') as TCastleScene;
  Viewport1 :=  DesignedComponent('Viewport1') as TCastleViewport;

  Button1 := DesignedComponent('Button1') as TCastleButton;
  Button2 := DesignedComponent('Button2') as TCastleButton;
  Button3 := DesignedComponent('Button3') as TCastleButton;
  Button4 := DesignedComponent('Button4') as TCastleButton;
  
  Button1.OnClick := @DoButton1Click;
  Button2.OnClick := @DoButton2Click;
  Button3.OnClick := @DoButton3Click;
  Button4.OnClick := @DoButton4Click;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TStateMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

procedure TStateMain.Render;
begin
  inherited;
  if not(AppTime = 0) then
    begin
      if Assigned(Scene1) and Scene1.IsVisible then
        begin
          AppTime := CastleGetTickCount64 - AppTime;
          WriteLnLog('Load time : ' + IntToStr(AppTime));
          AppTime := 0;
        end;
    end;
end;

procedure TStateMain.LoadModel(AUrl: String);
begin
  // Time the load
  AppTime := CastleGetTickCount64;

  if not(Scene1 = nil) then
    FreeAndNil(Scene1);
  Scene1 := TCastleScene.Create(Self);
  { Load Scene from AUrl}
  Scene1.Load(AUrl);
  { Normalize fits the model in a 1x1x1 cube and centers it }
  Scene1.Normalize;
  Scene1.PrepareResources([prRenderSelf], True, Viewport1.PrepareParams);
  Scene1.HeadlightOn := True;
  
  WriteLnLog('AnimationsList.Count : ' + IntToStr(Scene1.AnimationsList.Count));
  if Scene1.AnimationsList.Count > 0 then
    begin
      Scene1.PlayAnimation(Scene1.AnimationsList[0], True);
      WriteLnLog('Playing Animation : ' + Scene1.AnimationsList[0]);
    end;

  Viewport1.Items.MainScene := Scene1;
  Viewport1.Items.UseHeadlight := hlMainScene;
  Viewport1.Items.Add(Scene1);
  { ViewFromRadius sets the camera Radius distance from (0,0,0) 
    at Elevation pointing at (0,0,0) from Angle radians around 
    the Y-Axis - a random angle is used for the rotation to
    plainly indicate the model has changes
  }
  Viewport1.ViewFromRadius(sqrt(2), 0.81625, (2 * Pi * Random));

  LabelLoaded.Caption := 'Model loaded from ' + AUrl;
end;

procedure TStateMain.DoButton1Click(Sender: TObject);
begin
  LoadModel('castle-data:/oblique.glb');
end;

procedure TStateMain.DoButton2Click(Sender: TObject);
begin
  LoadModel(ZipFile.Protocol + '/Tree/models/tree.gltf');
end;

procedure TStateMain.DoButton3Click(Sender: TObject);
begin
  LoadModel(ZipStream.Protocol + '/scene.gltf');
end;

procedure TStateMain.DoButton4Click(Sender: TObject);
var
  TestStream: TStream;
  ExperimentalZipStream: TZipFileSystem;
  ExperimentalZipFile: TZipFileSystem;
begin
  TestStream := Download('castle-data:/square_shift.zip');
  ExperimentalZipStream := TZipFileSystem.Create(Self, TestStream, 'castle-data:/square_shift.zip');
  FreeAndNil(TestStream);

  RePackZipFile('experiment.zip', ExperimentalZipStream);
  FreeAndNil(ExperimentalZipStream);
  
  ExperimentalZipFile := TZipFileSystem.Create(Self, 'experiment.zip');
  LoadModel(ExperimentalZipFile.Protocol + '/scene.gltf');
  FreeAndNil(ExperimentalZipFile);
end;

function TStateMain.RePackZipFile(const AZipFilename: String; AZIpFileSystem: TZipFileSystem): Boolean;
var
  OurZipper: TZipper;
  i: Integer;
  fe: TZipFileEntry;
  ne: TZipFileEntry;
begin
  Result := false;
  if FileExists(AZipFilename) then
    DeleteFile(AZipFilename);

  OurZipper := TZipper.Create;
  OurZipper.OnStartFile:=@DoStartFile;
  OurZipper.OnEndFile:=@DoEndFile;
  OurZipper.FileName := AZipFileName;
  try
    for i := 0 to AZipFileSystem.RawFiles.Count -1 do
      begin
        fe := AZipFileSystem.RawFiles.Objects[i] as TZipFileEntry;
        WriteLnLog('Trying to add ' + fe.ArchiveFileName + ' to ' + AZipFilename);
        ne := OurZipper.Entries.Add as TZipFileEntry;
        if fe.IsDirectory then
          ne.Stream := nil
        else
          ne.Stream := AZipFileSystem.GetStream(fe.ArchiveFileName);
        ne.ArchiveFileName := fe.ArchiveFileName;
        ne.UTF8ArchiveFileName := fe.UTF8ArchiveFileName;
        ne.DiskFileName := fe.DiskFileName;
        ne.UTF8DiskFileName := fe.UTF8DiskFileName;
        ne.Size := fe.Size;
        ne.DateTime := fe.DateTime;
        ne.OS := fe.OS;
        ne.Attributes := fe.Attributes;
        ne.CompressionLevel := fe.CompressionLevel;
      end;

    WriteLnLog(' -> Start Zipping Files');
    OurZipper.ZipAllFiles;
    WriteLnLog(' -> Finished Zipping Files');
    Result := true;
  finally
    OurZipper.Free;
  end;
end; 

procedure TStateMain.DoEndFile(Sender : TObject; Const Ratio : Double);
begin
  WriteLnLog(Sender.ClassName + ' - Ratio : ' + FloatToStr(Ratio));
end;

procedure TStateMain.DoStartFile(Sender : TObject; Const AFileName : String);
begin
  WriteLnLog(Sender.ClassName + ' - File : ' + AFileName);
end;


end.
