"
My instances implement OSWindowFormRenderer interface for OSVMWindowHandle.
"
Class {
	#name : #OSVMFormRenderer,
	#superclass : #OSWindowFormRenderer,
	#category : 'OSWindow-VM'
}

{ #category : #private }
OSVMFormRenderer >> beDisplay [
	Smalltalk specialObjectsArray at: 15 put: form
]

{ #category : #'morphic integration' }
OSVMFormRenderer >> deferUpdatesWhile: aBlock [

	self primitiveDeferUpdates: true.
	aBlock ensure: [  self primitiveDeferUpdates: false. ]
]

{ #category : #other }
OSVMFormRenderer >> forceDisplayUpdate [
	<primitive: 231>
	"do nothing if primitive fails"
]

{ #category : #accessing }
OSVMFormRenderer >> form: aForm [
	super form: aForm.
	self beDisplay
]

{ #category : #private }
OSVMFormRenderer >> primRetryShowRectLeft: l right: r top: t bottom: b [
	"Copy the given rectangular section of the Display to to the screen. This primitive is not implemented on all platforms. Do nothing if it fails. "

	<primitive: 127>
	"do nothing if primitive fails"

]

{ #category : #private }
OSVMFormRenderer >> primShowRectLeft: l right: r top: t bottom: b [
	"Copy the given rectangular section of the Display to to the screen. This primitive is not implemented on all platforms. If this fails, retry integer coordinates."

	<primitive: 127>
	"if this fails, coerce coordinates to integers and try again"
	self primRetryShowRectLeft: l truncated
		right: r rounded
		top: t truncated
		bottom: b rounded.

]

{ #category : #deferring }
OSVMFormRenderer >> primitiveDeferUpdates: aBoolean [
	"Set the deferUpdates flag in the virtual machine. When this flag is true, BitBlt operations on the Display are not automatically propagated to the screen. If this underlying platform does not support deferred updates, this primitive will fail. Answer the receiver if the primitive succeeds, nil if it fails."

	<primitive: 126>
	^ nil  "answer nil if primitive fails"

]

{ #category : #'updating screen' }
OSVMFormRenderer >> updateAll [
	self updateRectangle: (0@0 extent: form extent)
]

{ #category : #'morphic integration' }
OSVMFormRenderer >> updateAreas: allDamage immediate: forceToScreen [

	"Force all the damage rects to the screen."
	allDamage do: [ :r| self updateRectangle: r ].
	
	 
]

{ #category : #'updating screen' }
OSVMFormRenderer >> updateRectangle: aRectangle [
	self primShowRectLeft: aRectangle left truncated
		right: aRectangle right rounded
		top: aRectangle top truncated
		bottom: aRectangle bottom rounded.

]
