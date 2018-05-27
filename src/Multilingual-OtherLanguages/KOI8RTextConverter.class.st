"
A KOI8RTextConverter class is used to convert cyrillic inputs.
"
Class {
	#name : #KOI8RTextConverter,
	#superclass : #ByteTextConverter,
	#category : #'Multilingual-OtherLanguages-TextConverter'
}

{ #category : #initialization }
KOI8RTextConverter class >> byteToUnicodeSpec [
	"ByteTextConverter generateByteToUnicodeSpec: 'http://unicode.org/Public/MAPPINGS/VENDORS/MISC/KOI8-R.TXT'"

	^ #(
	16r2500 16r2502 16r250C 16r2510 16r2514 16r2518 16r251C 16r2524 
	16r252C 16r2534 16r253C 16r2580 16r2584 16r2588 16r258C 16r2590 
	16r2591 16r2592 16r2593 16r2320 16r25A0 16r2219 16r221A 16r2248 
	16r2264 16r2265 16r00A0 16r2321 16r00B0 16r00B2 16r00B7 16r00F7 
	16r2550 16r2551 16r2552 16r0451 16r2553 16r2554 16r2555 16r2556 
	16r2557 16r2558 16r2559 16r255A 16r255B 16r255C 16r255D 16r255E 
	16r255F 16r2560 16r2561 16r0401 16r2562 16r2563 16r2564 16r2565 
	16r2566 16r2567 16r2568 16r2569 16r256A 16r256B 16r256C 16r00A9 
	16r044E 16r0430 16r0431 16r0446 16r0434 16r0435 16r0444 16r0433 
	16r0445 16r0438 16r0439 16r043A 16r043B 16r043C 16r043D 16r043E 
	16r043F 16r044F 16r0440 16r0441 16r0442 16r0443 16r0436 16r0432 
	16r044C 16r044B 16r0437 16r0448 16r044D 16r0449 16r0447 16r044A 
	16r042E 16r0410 16r0411 16r0426 16r0414 16r0415 16r0424 16r0413 
	16r0425 16r0418 16r0419 16r041A 16r041B 16r041C 16r041D 16r041E 
	16r041F 16r042F 16r0420 16r0421 16r0422 16r0423 16r0416 16r0412 
	16r042C 16r042B 16r0417 16r0428 16r042D 16r0429 16r0427 16r042A )
]

{ #category : #accessing }
KOI8RTextConverter class >> encodingNames [ 

	^ #('koi8-r') copy

]

{ #category : #initialization }
KOI8RTextConverter class >> initialize [
	self initializeTables
]

{ #category : #accessing }
KOI8RTextConverter class >> languageEnvironment [
	^RussianEnvironment
]
