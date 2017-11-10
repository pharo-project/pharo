Extension { #name : #BlockClosure }

{ #category : #'*metacello-core-scripting' }
BlockClosure >> execute: projectSpecBlock against: aScriptExecutor [
    aScriptExecutor executeBlock: self do: projectSpecBlock
]

{ #category : #'*metacello-core' }
BlockClosure >> setAuthorInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setAuthorWithBlock: self
]

{ #category : #'*metacello-core' }
BlockClosure >> setBaseline: aString withInMetacelloConfig: aMetacelloConfig [
    aMetacelloConfig setBaseline: aString withBlock: self
]

{ #category : #'*metacello-core' }
BlockClosure >> setBlessingInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setBlessingWithBlock: self
]

{ #category : #'*metacello-core' }
BlockClosure >> setConfiguration: aString withInMetacelloConfig: aMetacelloConfig [
    aMetacelloConfig setConfiguration: aString withBlock: self
]

{ #category : #'*metacello-core' }
BlockClosure >> setDescriptionInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setDescriptionWithBlock: self
]

{ #category : #'*metacello-core' }
BlockClosure >> setPackage: aString withInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setPackage: aString withBlock: self
]

{ #category : #'*metacello-core' }
BlockClosure >> setProject: aString withInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setProject: aString withBlock: self
]

{ #category : #'*metacello-core' }
BlockClosure >> setTimestampInMetacelloConfig: aMetacelloConfig [

	aMetacelloConfig setTimestampWithBlock: self
]
