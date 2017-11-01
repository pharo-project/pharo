"
#define LF_FULLFACESIZE 64
typedef struct tagENUMLOGFONTEX {
  LOGFONT  elfLogFont;
  TCHAR  elfFullName[LF_FULLFACESIZE];
  TCHAR  elfStyle[LF_FACESIZE];
  TCHAR  elfScript[LF_FACESIZE];
} ENUMLOGFONTEX, *LPENUMLOGFONTEX;

"
Class {
	#name : #ENUMLOGFONTEXA,
	#superclass : #LOGFONTA,
	#type : #bytes,
	#category : 'Alien-Win32'
}

{ #category : #'instance creation' }
ENUMLOGFONTEXA class >> dataSize [
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
]

{ #category : #accessing }
ENUMLOGFONTEXA >> elfFullName [
	^self strcpyFrom: 61
]
