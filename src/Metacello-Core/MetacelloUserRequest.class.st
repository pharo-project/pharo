"
I stand for whoever ran the script, as a requester: I ask for the roots of a load (see requestFor:). I am also the request of the findings about the load as a whole, such as a load with no root: I keep them, as a MetacelloProjectRequest keeps its own.
"
Class {
	#name : 'MetacelloUserRequest',
	#superclass : 'Object',
	#instVars : [
		'findings'
	],
	#category : 'Metacello-Core-Resolution',
	#package : 'Metacello-Core',
	#tag : 'Resolution'
}

{ #category : 'findings' }
MetacelloUserRequest >> addFinding: aFinding [

	findings add: aFinding.
	^ aFinding
]

{ #category : 'requests' }
MetacelloUserRequest >> chainNames [
	"The names of the projects that led to what the script asks for: none, nobody asked before the script"

	^ #(  )
]

{ #category : 'findings' }
MetacelloUserRequest >> findings [
	"What was found about the load as a whole"

	^ findings
]

{ #category : 'initialization' }
MetacelloUserRequest >> initialize [

	super initialize.
	findings := OrderedCollection new
]

{ #category : 'accessing' }
MetacelloUserRequest >> name [

	^ 'the script'
]

{ #category : 'printing' }
MetacelloUserRequest >> printOn: aStream [

	aStream nextPutAll: self name
]

{ #category : 'requests' }
MetacelloUserRequest >> requestFor: aProjectSpec [
	"My request for the root aProjectSpec names"

	^ MetacelloProjectRequest requester: self projectSpec: aProjectSpec
]
