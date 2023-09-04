"
This is the error signaled when a symbol is not found.
The error shows the symbol name and the module.
If the module is nil, we assume is the main module of the application
"
Class {
	#name : #SymbolNotFoundError,
	#superclass : #Error,
	#instVars : [
		'symbolName',
		'moduleName'
	],
	#category : #'FFI-Kernel'
}

{ #category : #'instance creation' }
SymbolNotFoundError class >> symbolName: aSymbolName module: aModuleName [ 
	
	^ self new
		symbolName: aSymbolName;
		moduleName: aModuleName;
		signal.
]

{ #category : #accessing }
SymbolNotFoundError >> messageText [

	^ messageText ifNil: [ messageText := self standardMessageText ]
]

{ #category : #accessing }
SymbolNotFoundError >> moduleName: anUndefinedObject [ 
	moduleName := anUndefinedObject
]

{ #category : #accessing }
SymbolNotFoundError >> standardMessageText [

	^ String streamContents: [ :s |
		s 
			nextPutAll: 'Could not find symbol named: ';
			print: symbolName.
		moduleName 
			ifNil: [ s nextPutAll: ' searching in main module' ]
			ifNotNil: [ 
				s nextPutAll: ' searching in module: '.
				s print: moduleName	
		]].
]

{ #category : #accessing }
SymbolNotFoundError >> symbolName: aString [ 
	symbolName := aString
]
