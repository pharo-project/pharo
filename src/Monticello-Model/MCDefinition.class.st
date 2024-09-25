"
A MCDefinition is the root of inheritance of entities representing code. 
"
Class {
	#name : 'MCDefinition',
	#superclass : 'Object',
	#category : 'Monticello-Model',
	#package : 'Monticello-Model'
}

{ #category : 'comparing' }
MCDefinition >> <= other [
	^ self sortKey <= other sortKey
]

{ #category : 'comparing' }
MCDefinition >> = aDefinition [
	^(aDefinition isKindOf: MCDefinition) and: [self isRevisionOf: aDefinition]
]

{ #category : 'accessing' }
MCDefinition >> actualClass [
	"Since the targetClass call on a patch operation will fail
	otherwise."
	
	^nil
]

{ #category : 'accessing' }
MCDefinition >> className [
	"Answer the class name here or nil if not applicable."
	
	^nil
]

{ #category : 'accessing' }
MCDefinition >> definition [

	^ self
]

{ #category : 'accessing' }
MCDefinition >> description [
	self subclassResponsibility
]

{ #category : 'accessing' }
MCDefinition >> diffSource [
	^ self source
]

{ #category : 'accessing' }
MCDefinition >> fullClassName [
	"Answer the className by default."
	
	^self className
]

{ #category : 'comparing' }
MCDefinition >> hash [
	^ self description hash
]

{ #category : 'testing' }
MCDefinition >> isClassDefinition [
	^false
]

{ #category : 'testing' }
MCDefinition >> isClassDefinitionExtension [
	"Answer true if this definition extends the regular class definition"
	^false
]

{ #category : 'testing' }
MCDefinition >> isMethodDefinition [
	^false
]

{ #category : 'testing' }
MCDefinition >> isOrganizationDefinition [
	^false
]

{ #category : 'comparing' }
MCDefinition >> isRevisionOf: aDefinition [
	^ (aDefinition isKindOf: MCDefinition) and: [aDefinition description = self description]
]

{ #category : 'comparing' }
MCDefinition >> isSameRevisionAs: aDefinition [
	^ self = aDefinition
]

{ #category : 'testing' }
MCDefinition >> isScriptDefinition [
	^false
]

{ #category : 'testing' }
MCDefinition >> isTraitDefinition [
	^ false
]

{ #category : 'installing' }
MCDefinition >> load [
	
]

{ #category : 'installing' }
MCDefinition >> loadOver: aDefinition [
	self load
	
]

{ #category : 'installing' }
MCDefinition >> postload [
]

{ #category : 'installing' }
MCDefinition >> postloadOver: aDefinition [
	self postload
]

{ #category : 'printing' }
MCDefinition >> printOn: aStream [
	super printOn: aStream.
	aStream nextPutAll: '(', self summary, ')'
]

{ #category : 'accessing' }
MCDefinition >> provisions [
	^ #()
]

{ #category : 'accessing' }
MCDefinition >> requirements [
	^ #()
]

{ #category : 'comparing' }
MCDefinition >> sortKey [
	self subclassResponsibility 
]

{ #category : 'accessing' }
MCDefinition >> source [
	^ self subclassResponsibility.
]

{ #category : 'printing' }
MCDefinition >> summary [
	self subclassResponsibility 
]

{ #category : 'printing' }
MCDefinition >> summarySuffixOver: previousDefinition [
	^self source = previousDefinition source
		ifTrue: [ ' (source same but rev changed)' ]
		ifFalse: [ ' (changed)' ]
]

{ #category : 'installing' }
MCDefinition >> unload [
]
