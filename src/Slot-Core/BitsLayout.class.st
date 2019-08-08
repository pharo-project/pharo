"
I am a specialized layout which does not hold slots but only raw data (bytes or words).
"
Class {
	#name : #BitsLayout,
	#superclass : #ObjectLayout,
	#category : #'Slot-Core-Layout'
}

{ #category : #accessing }
BitsLayout >> bytesPerSlot [

	^ 8
]

{ #category : #extending }
BitsLayout >> extend [
	"Answer a default extension of me."

	^ self species new 
]

{ #category : #extending }
BitsLayout >> extendByte [
	IncompatibleLayoutConflict new
		layout: self;
		subType: #byte;
		signal
]

{ #category : #extending }
BitsLayout >> extendDoubleByte [
	IncompatibleLayoutConflict new
		layout: self;
		subType: #doubleByte;
		signal
]

{ #category : #extending }
BitsLayout >> extendDoubleWord [
	IncompatibleLayoutConflict new
		layout: self;
		subType: #doubleWord;
		signal
]

{ #category : #extending }
BitsLayout >> extendWeak: aLayoutClassScope [ 
	IncompatibleLayoutConflict new
		layout: self;
		subType: #weak;
		signal
]

{ #category : #extending }
BitsLayout >> extendWord [
	IncompatibleLayoutConflict new
		layout: self;
		subType: #word;
		signal
]

{ #category : #testing }
BitsLayout >> isBits [
	^ true
]

{ #category : #testing }
BitsLayout >> isBytes [
	^ false
]

{ #category : #testing }
BitsLayout >> isDoubleBytes [
	^ false
]

{ #category : #testing }
BitsLayout >> isDoubleWords [
	^ false
]

{ #category : #testing }
BitsLayout >> isVariable [
	^ true
]

{ #category : #testing }
BitsLayout >> isWords [
	^ false
]
