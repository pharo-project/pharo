Extension { #name : #OSWindow }

{ #category : #'*OSWindow-VM' }
OSWindow class >> resetVMWindow [
	"self resetVMWindow"
	VMWindow := nil.
	

]

{ #category : #'*OSWindow-VM' }
OSWindow class >> vmWindow [
	| attributes |
	^ VMWindow ifNil: [ 
		attributes := OSWindowAttributes new.
		attributes preferableDriver: VMWindowDriver new.
		attributes extent: 640@480.
		VMWindow := OSWindow createWithAttributes: attributes
	] 
]
