Class {
	#name : #RFReflectivityASTCacheStrategy,
	#superclass : #Object,
	#category : #'Reflectivity-Compiler'
}

{ #category : #accessing }
RFReflectivityASTCacheStrategy >> getASTFor: aCompiledMethod [
	^ aCompiledMethod reflectiveMethod
		ifNil: [ aCompiledMethod parseTree
				doSemanticAnalysisIn: aCompiledMethod methodClass ]
		ifNotNil: [ :rfMethod | rfMethod ast ]
]
