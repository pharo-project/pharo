instance creation
dataSize
	^188
	
"
#define LF_FULLFACESIZE 64
typedef struct tagENUMLOGFONTEX {
  LOGFONT  elfLogFont;
  TCHAR  elfFullName[LF_FULLFACESIZE];
  TCHAR  elfStyle[LF_FACESIZE];
  TCHAR  elfScript[LF_FACESIZE];
} ENUMLOGFONTEX, *LPENUMLOGFONTEX;

"