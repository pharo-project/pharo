Class {
	#name : #SpStyleSTONReader,
	#superclass : #STONReader,
	#instVars : [
		'currentStyle'
	],
	#category : #'Spec2-Adapters-Morphic-StyleSheet'
}

{ #category : #'instance creation' }
SpStyleSTONReader class >> fromStream: readStream [

	^ (self on: readStream) next
]

{ #category : #'instance creation' }
SpStyleSTONReader class >> fromString: aString [

	^ self fromStream: aString readStream
]

{ #category : #private }
SpStyleSTONReader >> isStyleClassChar: char [ 

	^ char = $.
]

{ #category : #private }
SpStyleSTONReader >> lookupClass: name [

	"The SpStyleSTONReader only looks for ston names in the hierarchy of SpStyle"
	
	^ classes 
		at: name 
		ifAbsentPut: [
			(SpStyle allSubclasses , Color withAllSubclasses) 
				detect: [ :class | class isMeta not and: [ class stonName = name ]  ]
				ifNone: [ NotFound signalFor: name ] ]
	
]

{ #category : #parsing }
SpStyleSTONReader >> parseSimpleValue [
	| char |

	readStream atEnd ifFalse: [ 
		char := readStream peek.
		(self isStyleClassChar: char) ifTrue: [ 
			^ self parseStyleClass ] ].

	^ super parseSimpleValue
]

{ #category : #parsing }
SpStyleSTONReader >> parseStyleClass [
	| name reference style |

	[
		reference := self newReference.
		name := self parseStyleName.
		style := self styleClass new
			name: name;
			parent: currentStyle;
			yourself.
		self 
			pushStyle: style 
			during: [ style fromSton: self ]. 
		self setReference: reference to: style ]
	on: NotFound 
	do: [ :notFound |
		acceptUnknownClasses 
			ifTrue: [ 
				style := STON mapClass new.
				self storeReference: style.
				self parseMapDo: [ :key :value |
					style at: key put: value ].
				style at: STON classNameKey put: notFound object ]
			ifFalse: [ self error: 'Cannot resolve class named ', notFound object printString ] ].
	
	^ style
]

{ #category : #'private parsing' }
SpStyleSTONReader >> parseStyleName [
	| styleName |

	readStream next. "ignore dot"
	styleName := self stringStreamContents: [ :stream |
		[ readStream atEnd not and: [ self isClassChar: readStream peek ] ] whileTrue: [ 
			stream nextPut: readStream next ] ].
	self consumeWhitespace.
	^ styleName asString
]

{ #category : #accessing }
SpStyleSTONReader >> pushStyle: aStyle during: aBlock [
	| oldStyle |
	
	oldStyle := currentStyle.
	currentStyle := aStyle.
	aBlock ensure: [ 
		currentStyle := oldStyle ]
]

{ #category : #private }
SpStyleSTONReader >> styleClass [

	^ SpStyleClass
]
