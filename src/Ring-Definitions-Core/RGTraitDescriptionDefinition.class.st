"
RGTraitDescriptionDefinition is the common parent for traits and metatraits that know their users (ie. classes/metaclasses)
"
Class {
	#name : 'RGTraitDescriptionDefinition',
	#superclass : 'RGBehaviorDefinition',
	#instVars : [
		'users'
	],
	#category : 'Ring-Definitions-Core-Base',
	#package : 'Ring-Definitions-Core',
	#tag : 'Base'
}

{ #category : 'testing' }
RGTraitDescriptionDefinition class >> isAbstract [

	^ self == RGTraitDescriptionDefinition
]

{ #category : 'adding-removing users' }
RGTraitDescriptionDefinition >> addUser: aRGBehavior [

	self users add: aRGBehavior
]

{ #category : 'variables' }
RGTraitDescriptionDefinition >> allClassVarNames [

	^#()
]

{ #category : 'variables' }
RGTraitDescriptionDefinition >> allInstVarNames [

	^#()
]

{ #category : 'testing' }
RGTraitDescriptionDefinition >> isTrait [

	^true
]

{ #category : 'adding-removing users' }
RGTraitDescriptionDefinition >> removeUser: aRGBehavior [

	self users remove: aRGBehavior ifAbsent:[]
]

{ #category : 'accessing' }
RGTraitDescriptionDefinition >> traitUsers [
	"Retrieves the receiver's classes and traits"

	^users ifNil:[ users := OrderedCollection new ]
]

{ #category : 'accessing' }
RGTraitDescriptionDefinition >> traitUsers: aCollection [

	users:= aCollection
]

{ #category : 'accessing' }
RGTraitDescriptionDefinition >> users [
	"Retrieves the receiver's classes and traits"

	^users ifNil:[ users := OrderedCollection new ]
]

{ #category : 'accessing' }
RGTraitDescriptionDefinition >> users: aCollection [

	users:= aCollection
]
