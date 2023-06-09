"
A paint mode controls how the incoming color (source) will be transferred to surface (destination).

To set a paint mode for canvas, send a message with corresponding mode name to my instance.
My instance(s) is available via canvas protocol, i.e. 'canvas paintMode'.

For example, to set an 'over' paint mode, use following:

	canvas paintMode over.

If you want to restore original paint mode after performing drawing operations, which may change it, use #restoreAfter: method , i.e. 

	canvas paintMode restoreAfter: [
		.. perform any drawing operations here..
		].

Since different backends may support different set of paint modes, the default implementation in AthensPaintMode for 
all mode setters is to signal an error.
To query a set of available paint modes, supported by backend, use #availableModes message.


"
Class {
	#name : #AthensPaintMode,
	#superclass : #Object,
	#category : #'Athens-Core-Paints'
}

{ #category : #modes }
AthensPaintMode >> add [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> atop [
	"Set 'atop' painting mode "

	self notAvailable
]

{ #category : #capabilities }
AthensPaintMode >> availableModes [
	"Answer a collection of paint mode names, currently supported by backend.
	Different backends may support different sets of paint modes.

	You may need to structure your code depending on modes available"

	^ #()
]

{ #category : #modes }
AthensPaintMode >> clear [
	"Set 'clear' drawing mode.

	Drawing under this mode will clear the surface under the shape boundaries"

	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> colorBurn [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> colorDodge [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> darken [
	self notAvailable
]

{ #category : #'default mode' }
AthensPaintMode >> default [
	"Set the default paint mode. The default paint mode in Athens, is 'over' "
	^ self over
]

{ #category : #modes }
AthensPaintMode >> dest [
	"Set 'dest' drawing mode.

	Drawing under this mode will ignore the source, as if nothing drawn"

	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> destAtop [

	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> destIn [

	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> destOut [

	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> destOver [

	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> difference [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> exclusion [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> hardLight [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> hslColor [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> hslHue [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> hslLuminosity [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> hslSaturation [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> in [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> lighten [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> multiply [
	self notAvailable
]

{ #category : #errors }
AthensPaintMode >> notAvailable [
	^ self error: 'A requested paint mode is not supported by backend'
]

{ #category : #modes }
AthensPaintMode >> out [
	"Set 'out' painting mode "

	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> over [
	"Set 'over' drawing mode. This is default paint mode.

	Drawing under this mode will blend source with destination color using source alpha component"

	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> overlay [
	self notAvailable
]

{ #category : #convenience }
AthensPaintMode >> restoreAfter: aBlock [
	"Restore current paint mode after evaluating a block"

	self subclassResponsibility
]

{ #category : #modes }
AthensPaintMode >> saturate [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> screen [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> softLight [
	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> source [
	"Set 'source' drawing mode.

	Drawing under this mode will replace the content with incoming source under the shape boundaries"

	self notAvailable
]

{ #category : #modes }
AthensPaintMode >> xor [
	self notAvailable
]
