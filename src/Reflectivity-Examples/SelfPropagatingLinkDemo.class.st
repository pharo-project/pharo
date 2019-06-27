"
Small Demo of a Link that propages itself at runtime

instance :=SelfPropagatingLinkDemo new.

Now install on #factorial:
instance installOnAllSendsInMethod: (Integer>>#factorial)

if we now execute the code

    10 factorial

inspec the instance of SelfPropagatingLinkDemo to see which methods have been visited by the link
"
Class {
	#name : #SelfPropagatingLinkDemo,
	#superclass : #Object,
	#instVars : [
		'methodsvisited',
		'link'
	],
	#category : #'Reflectivity-Examples'
}

{ #category : #initialization }
SelfPropagatingLinkDemo >> initialize [

	methodsvisited := OrderedCollection new
]

{ #category : #displaying }
SelfPropagatingLinkDemo >> installOnAllSendsInMethod: aMethod [
	methodsvisited add: aMethod.
	aMethod sendNodes do: [ :sendNode | (sendNode hasMetalink: self link) ifFalse: [sendNode link: self link]]
]

{ #category : #displaying }
SelfPropagatingLinkDemo >> link [
	^link := link ifNil: [MetaLink new
		metaObject: self;
		selector: #installOnAllSendsInMethod:;
		arguments: #(methodToExecute)]
]

{ #category : #acccessing }
SelfPropagatingLinkDemo >> methodsvisited [
	^ methodsvisited
]
