class initialization
initialize
	"FT2Constants initialize"

	LoadDefault := 0.
	LoadNoScale := 1.
	LoadNoHinting := 2.
	LoadRender := 4.
	LoadNoBitmap := 8.
	LoadVerticalLayout := 16.
	LoadForceAutohint := 32.
	LoadCropBitmap := 64.
	LoadPedantic := 128.
	LoadIgnoreGlobalAdvanceWidth := 512.
	LoadNoRecurse := 1024.
	LoadIgnoreTransform := 2048.
	LoadMonochrome := 4096.
	LoadLinearDesign := 8192.
	LoadSbitsOnly := 16384.
	LoadNoAutohint := 32768.

	"One of these flags may be OR'd with the above."
	LoadTargetNormal := 0.
	LoadTargetLight := 1 bitShift: 16.
	LoadTargetMono := 2 bitShift: 16.
	LoadTargetLCD := 3 bitShift: 16.
	LoadTargetLCDV  := 4 bitShift: 16.

	"rendering mode constants"
	RenderModeNormal := 0.
	RenderModeLight := 1.
	RenderModeMono := 2.
	RenderModeLCD := 3.	
	RenderModeLCDV := 4.

	"pixel mode constants"
	PixelModeNone := 0.
	PixelModeMono := 1.
	PixelModeGray := 2.
	PixelModeGray2 := 3.
	PixelModeGray4 := 4.
	PixelModeLCD := 5.
	PixelModeLCDV := 6.
	
	StyleFlagItalic := 1.
	StyleFlagBold := 2.
