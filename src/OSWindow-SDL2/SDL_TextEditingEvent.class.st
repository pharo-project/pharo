"
# An Editing event

https://wiki.libsdl.org/SDL_TextEditingEvent

A structure that contains keyboard text editing event information.
Text editing events inform about text that was not _yet_ accepted by the user.
"
Class {
	#name : 'SDL_TextEditingEvent',
	#superclass : 'SDL2MappedEvent',
	#classVars : [
		'OFFSET_LENGTH',
		'OFFSET_START',
		'OFFSET_TEXT0',
		'OFFSET_TEXT1',
		'OFFSET_TEXT10',
		'OFFSET_TEXT11',
		'OFFSET_TEXT12',
		'OFFSET_TEXT13',
		'OFFSET_TEXT14',
		'OFFSET_TEXT15',
		'OFFSET_TEXT16',
		'OFFSET_TEXT17',
		'OFFSET_TEXT18',
		'OFFSET_TEXT19',
		'OFFSET_TEXT2',
		'OFFSET_TEXT20',
		'OFFSET_TEXT21',
		'OFFSET_TEXT22',
		'OFFSET_TEXT23',
		'OFFSET_TEXT24',
		'OFFSET_TEXT25',
		'OFFSET_TEXT26',
		'OFFSET_TEXT27',
		'OFFSET_TEXT28',
		'OFFSET_TEXT29',
		'OFFSET_TEXT3',
		'OFFSET_TEXT30',
		'OFFSET_TEXT31',
		'OFFSET_TEXT4',
		'OFFSET_TEXT5',
		'OFFSET_TEXT6',
		'OFFSET_TEXT7',
		'OFFSET_TEXT8',
		'OFFSET_TEXT9',
		'OFFSET_TIMESTAMP',
		'OFFSET_TYPE',
		'OFFSET_WINDOWID'
	],
	#category : 'OSWindow-SDL2-Bindings',
	#package : 'OSWindow-SDL2',
	#tag : 'Bindings'
}

{ #category : 'event type' }
SDL_TextEditingEvent class >> eventType [
	^ SDL_TEXTEDITING
]

{ #category : 'fields description' }
SDL_TextEditingEvent class >> fieldsDesc [
	"
	self initializeAccessors
	"
	^ #(
    Uint32 type;
    Uint32 timestamp;
    Uint32 windowID;

	"HACK: for char text[32];"
      char text0;
	char text1;
	char text2;
	char text3;
	char text4;
	char text5;
	char text6;
	char text7;
	char text8;
	char text9;
	char text10;
	char text11;
	char text12;
	char text13;
	char text14;
	char text15;
	char text16;
	char text17;
	char text18;
	char text19;
	char text20;
	char text21;
	char text22;
	char text23;
	char text24;
	char text25;
	char text26;
	char text27;
	char text28;
	char text29;
	char text30;
	char text31;

	Sint32 start; "the location to begin editing from"
	Sint32 length "the number of characters to edit from the start point

"
 	)
]

{ #category : 'visitor' }
SDL_TextEditingEvent >> accept: aVisitor [
	^ aVisitor visitTextEditingEvent: self
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> length [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_LENGTH
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> length: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_LENGTH put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> start [
	"This method was automatically generated"
	^handle signedLongAt: OFFSET_START
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> start: anObject [
	"This method was automatically generated"
	handle signedLongAt: OFFSET_START put: anObject
]

{ #category : 'accessing' }
SDL_TextEditingEvent >> strlen [

	| len |
	len := 0.
	12 to: 12 + 32 - 1 do: [ :i |
		(self getHandle uint8AtOffset: i) = 0 ifTrue: [ ^ len ].
		len := len + 1 ].
	^ len
]

{ #category : 'accessing' }
SDL_TextEditingEvent >> text [

	| len text |
	len := self strlen.
	text := ByteArray new: len.
	1 to: len do: [ :i | text at: i put: (self getHandle uint8AtOffset: 12 + i - 1) ].
	^ text
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text0 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT0
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text0: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT0 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text1 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT1
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text10 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT10
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text10: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT10 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text11 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT11
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text11: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT11 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text12 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT12
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text12: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT12 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text13 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT13
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text13: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT13 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text14 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT14
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text14: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT14 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text15 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT15
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text15: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT15 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text16 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT16
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text16: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT16 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text17 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT17
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text17: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT17 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text18 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT18
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text18: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT18 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text19 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT19
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text19: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT19 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text1: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT1 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text2 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT2
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text20 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT20
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text20: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT20 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text21 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT21
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text21: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT21 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text22 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT22
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text22: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT22 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text23 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT23
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text23: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT23 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text24 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT24
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text24: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT24 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text25 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT25
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text25: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT25 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text26 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT26
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text26: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT26 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text27 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT27
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text27: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT27 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text28 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT28
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text28: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT28 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text29 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT29
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text29: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT29 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text2: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT2 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text3 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT3
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text30 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT30
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text30: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT30 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text31 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT31
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text31: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT31 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text3: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT3 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text4 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT4
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text4: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT4 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text5 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT5
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text5: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT5 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text6 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT6
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text6: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT6 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text7 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT7
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text7: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT7 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text8 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT8
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text8: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT8 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text9 [
	"This method was automatically generated"
	^handle unsignedCharAt: OFFSET_TEXT9
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> text9: anObject [
	"This method was automatically generated"
	handle unsignedCharAt: OFFSET_TEXT9 put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> timestamp [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TIMESTAMP
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> timestamp: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TIMESTAMP put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> type [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_TYPE
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> type: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_TYPE put: anObject
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> windowID [
	"This method was automatically generated"
	^handle unsignedLongAt: OFFSET_WINDOWID
]

{ #category : 'accessing structure variables' }
SDL_TextEditingEvent >> windowID: anObject [
	"This method was automatically generated"
	handle unsignedLongAt: OFFSET_WINDOWID put: anObject
]
