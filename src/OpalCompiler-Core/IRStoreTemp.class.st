"
store temp
"
Class {
	#name : 'IRStoreTemp',
	#superclass : 'IRTempAccess',
	#category : 'OpalCompiler-Core-IR-Nodes',
	#package : 'OpalCompiler-Core',
	#tag : 'IR-Nodes'
}

{ #category : 'visiting' }
IRStoreTemp >> accept: arg1 [

	^ arg1 visitStoreTemp: self
]

{ #category : 'testing' }
IRStoreTemp >> isStore [

	^ true
]

{ #category : 'accessing' }
IRStoreTemp >> popIntoNode [

	| tmp2 |
	tmp2 := IRPopIntoTemp new.
	tmp2 name: name.
	^ tmp2 yourself
]
