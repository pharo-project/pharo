Class {
	#name : 'MetacelloLinearLoadingStrategy',
	#superclass : 'MetacelloLoadingStrategy',
	#category : 'Metacello-Core-Directives',
	#package : 'Metacello-Core',
	#tag : 'Directives'
}

{ #category : 'switch-strategy' }
MetacelloLinearLoadingStrategy >> changeToAtomicWithLoader: aLoader [
	"If I'm linear, just open a new atomic one"

	^ MetacelloAtomicLoadingStrategy new
		  gofer: gofer;
		  loader: aLoader;
		  yourself
]

{ #category : 'switch-strategy' }
MetacelloLinearLoadingStrategy >> changeToLinearWithLoader: aLoader [
	"If I'm linear, just do nothing and continue as is"

	^ self
]

{ #category : 'actions' }
MetacelloLinearLoadingStrategy >> executePackageDirective: aMetacelloPackageLoadDirective [

	aMetacelloPackageLoadDirective loader loadingSpecLoader
		loadPackageDirectives: { aMetacelloPackageLoadDirective }
		gofer: self gofer
]

{ #category : 'loading' }
MetacelloLinearLoadingStrategy >> executePostloadDirective: aPostloadDirective [

	aPostloadDirective spec postLoadDoItBlock ifNotNil: [ :block |
		MetacelloNotification signal: 'Evaluating -> ' , aPostloadDirective spec label , ' >> ' , aPostloadDirective spec postLoadDoIt value asString.
		aPostloadDirective evaluateSupplyingAnswers: [ block valueWithPossibleArgs: (Array with: aPostloadDirective loader with: aPostloadDirective spec) ].
		MetacelloNotification signal: 'Evaluated -> ' , aPostloadDirective spec label , ' >> ' , aPostloadDirective spec postLoadDoIt value asString ]
]

{ #category : 'loading' }
MetacelloLinearLoadingStrategy >> executePreloadDirective: aPreloadDirective [

	aPreloadDirective spec preLoadDoItBlock ifNotNil: [ :block |
		MetacelloNotification signal: 'Evaluating -> ' , aPreloadDirective spec label , ' >> ' , aPreloadDirective spec preLoadDoIt value asString.
		aPreloadDirective evaluateSupplyingAnswers: [ block valueWithPossibleArgs: (Array with: aPreloadDirective loader with: aPreloadDirective spec) ].
		MetacelloNotification signal: 'Evaluated -> ' , aPreloadDirective spec label , ' >> ' , aPreloadDirective spec preLoadDoIt value asString ]
]

{ #category : 'actions' }
MetacelloLinearLoadingStrategy >> finalizeLoadWith: anObject [ 
	
	"Do nothing"
]
