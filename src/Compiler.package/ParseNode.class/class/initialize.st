class initialization
initialize
	"ParseNode initialize. VariableNode initialize"
	LdInstType := 1.
	LdTempType := 2.
	LdLitType := 3.
	LdLitIndType := 4.
	SendType := 5.
	CodeBases := #(0 16 32 64 208 ).
	CodeLimits := #(16 16 32 32 16 ).
	LdSelf := 112.
	LdTrue := 113.
	LdFalse := 114.
	LdNil := 115.
	LdMinus1 := 116.
	LoadLong := 128.
	Store := 129.
	StorePop := 130.
	ShortStoP := 96.
	SendLong := 131.
	DblExtDoAll := 132.
	SendLong2 := 134.
	LdSuper := 133.
	Pop := 135.
	Dup := 136.
	LdThisContext := 137.
	EndMethod := 124.
	EndRemote := 125.
	Jmp := 144.
	Bfp := 152.
	JmpLimit := 8.
	JmpLong := 164.  "code for jmp 0"
	BtpLong := 168.
	SendPlus := 176.
	Send := 208.
	SendLimit := 16