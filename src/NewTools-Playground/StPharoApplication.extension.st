Extension { #name : #StPharoApplication }

{ #category : #'*NewTools-Playground' }
StPharoApplication >> addSharedBinding: aBinding [
	
	self sharedBindings add: aBinding
]

{ #category : #'*NewTools-Playground' }
StPharoApplication >> removeSharedBinding: aBinding [

	self sharedBindings removeKey: aBinding key
]

{ #category : #'*NewTools-Playground' }
StPharoApplication >> sharedBindings [
	"bindings that are shared by all playgrounds"
	
	^ self 
		propertyAt: #sharedBindings 
		ifAbsentPut: [ Dictionary new ]
]
