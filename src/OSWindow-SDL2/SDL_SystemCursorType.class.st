"
I am the FFIEnumeration used to get the system cursors. I am defined as following C Enum

```
typedef enum {
        SDL_SYSTEM_CURSOR_ARROW,     
        SDL_SYSTEM_CURSOR_IBEAM,     
        SDL_SYSTEM_CURSOR_WAIT,      
        SDL_SYSTEM_CURSOR_CROSSHAIR, 
        SDL_SYSTEM_CURSOR_WAITARROW, 
        SDL_SYSTEM_CURSOR_SIZENWSE,  
        SDL_SYSTEM_CURSOR_SIZENESW,  
        SDL_SYSTEM_CURSOR_SIZEWE,    
        SDL_SYSTEM_CURSOR_SIZENS,    
        SDL_SYSTEM_CURSOR_SIZEALL,   
        SDL_SYSTEM_CURSOR_NO,        
        SDL_SYSTEM_CURSOR_HAND,      
        SDL_NUM_SYSTEM_CURSORS
} SDL_SystemCursor; 
```
"
Class {
	#name : #'SDL_SystemCursorType',
	#superclass : #FFIEnumeration,
	#classVars : [
		'SDL_NUM_SYSTEM_CURSORS',
		'SDL_SYSTEM_CURSOR_ARROW',
		'SDL_SYSTEM_CURSOR_CROSSHAIR',
		'SDL_SYSTEM_CURSOR_HAND',
		'SDL_SYSTEM_CURSOR_IBEAM',
		'SDL_SYSTEM_CURSOR_NO',
		'SDL_SYSTEM_CURSOR_SIZEALL',
		'SDL_SYSTEM_CURSOR_SIZENESW',
		'SDL_SYSTEM_CURSOR_SIZENS',
		'SDL_SYSTEM_CURSOR_SIZENWSE',
		'SDL_SYSTEM_CURSOR_SIZEWE',
		'SDL_SYSTEM_CURSOR_WAIT',
		'SDL_SYSTEM_CURSOR_WAITARROW'
	],
	#category : #'OSWindow-SDL2-Bindings'
}

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_NUM_SYSTEM_CURSORS [
	^ SDL_NUM_SYSTEM_CURSORS
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_ARROW [
	^ SDL_SYSTEM_CURSOR_ARROW
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_CROSSHAIR [
	^ SDL_SYSTEM_CURSOR_CROSSHAIR
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_HAND [
	^ SDL_SYSTEM_CURSOR_HAND
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_IBEAM [
	^ SDL_SYSTEM_CURSOR_IBEAM
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_NO [
	^ SDL_SYSTEM_CURSOR_NO
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_SIZEALL [
	^ SDL_SYSTEM_CURSOR_SIZEALL
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_SIZENESW [
	^ SDL_SYSTEM_CURSOR_SIZENESW
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_SIZENS [
	^ SDL_SYSTEM_CURSOR_SIZENS
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_SIZENWSE [
	^ SDL_SYSTEM_CURSOR_SIZENWSE
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_SIZEWE [
	^ SDL_SYSTEM_CURSOR_SIZEWE
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_WAIT [
	^ SDL_SYSTEM_CURSOR_WAIT
]

{ #category : #accessing }
SDL_SystemCursorType class >> SDL_SYSTEM_CURSOR_WAITARROW [
	^ SDL_SYSTEM_CURSOR_WAITARROW
]

{ #category : #'enum declaration' }
SDL_SystemCursorType class >> enumDecl [ 

	^ #( SDL_SYSTEM_CURSOR_ARROW 			0     
        SDL_SYSTEM_CURSOR_IBEAM 			1     
        SDL_SYSTEM_CURSOR_WAIT 			2     
        SDL_SYSTEM_CURSOR_CROSSHAIR 		3 
        SDL_SYSTEM_CURSOR_WAITARROW 		4 
        SDL_SYSTEM_CURSOR_SIZENWSE 		5  
        SDL_SYSTEM_CURSOR_SIZENESW 		6  
        SDL_SYSTEM_CURSOR_SIZEWE 			7    
        SDL_SYSTEM_CURSOR_SIZENS 			8    
        SDL_SYSTEM_CURSOR_SIZEALL 		9   
        SDL_SYSTEM_CURSOR_NO 				10        
        SDL_SYSTEM_CURSOR_HAND 			11      
        SDL_NUM_SYSTEM_CURSORS				12)
]

{ #category : #'class initialization' }
SDL_SystemCursorType class >> initialize [
	self initializeEnumeration.
	
	self classPool keysDo: [ :e |
		self class compile: ('{1}
	^ {1}' format: {e asString}) classified: #accessing	] 

]

{ #category : #printing }
SDL_SystemCursorType >> printOn: stream [
	super printOn: stream.
	stream nextPut: $(;
		 print: self item ;
		 space;
		 print: self value;
		 nextPut: $)
]
