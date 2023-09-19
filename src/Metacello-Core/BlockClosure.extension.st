Extension { #name : 'BlockClosure' }

{ #category : '*Metacello-Core' }
BlockClosure >> execute: projectSpecBlock against: aScriptExecutor [
    aScriptExecutor executeBlock: self do: projectSpecBlock
]

{ #category : '*Metacello-Core' }
BlockClosure >> setAuthorInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setAuthorWithBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setBaseline: aString withInMetacelloConfig: aMetacelloConfig [
    aMetacelloConfig setBaseline: aString withBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setBlessingInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setBlessingWithBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setConfiguration: aString withInMetacelloConfig: aMetacelloConfig [
    aMetacelloConfig setConfiguration: aString withBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setDescriptionInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setDescriptionWithBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setPackage: aString withInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setPackage: aString withBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setProject: aString withInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setProject: aString withBlock: self
]

{ #category : '*Metacello-Core' }
BlockClosure >> setTimestampInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setTimestampWithBlock: self
]
