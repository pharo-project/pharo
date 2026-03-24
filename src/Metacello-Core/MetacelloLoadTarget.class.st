Class {
	#name : 'MetacelloLoadTarget',
	#superclass : 'MetacelloTarget',
	#category : 'Metacello-Core-Targets',
	#package : 'Metacello-Core',
	#tag : 'Targets'
}

{ #category : 'accessing' }
MetacelloLoadTarget >> baselineFor: aDirective [

	^ aDirective spec project baseline
]

{ #category : 'accessing' }
MetacelloLoadTarget >> loader [
	
	^ engine loader
]

{ #category : 'visiting' }
MetacelloLoadTarget >> visitAtomicLoadDirective: aMetacelloAtomicLoadDirective [ 
	
	| atomicLoader |
	atomicLoader := MetacelloAtomicLoad new.
	atomicLoader target: self.
	aMetacelloAtomicLoadDirective loadDirectives do: [ :dir |
		dir acceptVisitor: atomicLoader ].
	atomicLoader finishLoading.
]

{ #category : 'visiting' }
MetacelloLoadTarget >> visitLinearLoadDirective: aMetacelloLinearLoadDirective [

	aMetacelloLinearLoadDirective loadDirectives do: [ :dir |
		dir acceptVisitor: self ]
]

{ #category : 'visiting' }
MetacelloLoadTarget >> visitPackageLoadDirective: aMetacelloPackageLoadDirective [

	engine loader
		loadAtomicPackageDirectives: { aMetacelloPackageLoadDirective }
		engine: engine
]

{ #category : 'visiting' }
MetacelloLoadTarget >> visitPostLoadDirective: aMetacelloPostLoadDirective [

	aMetacelloPostLoadDirective spec postLoadDoIt ifNotNil: [ :symbol |
			MetacelloNotification signal: 'Executing: ' , aMetacelloPostLoadDirective label.
			
			MetacelloNotification 
				logTimeDuring: [
						symbol valueWithEnoughArguments: {
								(self baselineFor: aMetacelloPostLoadDirective).
								self.
								aMetacelloPostLoadDirective spec } ] 
				description: 'Done executing: ' , aMetacelloPostLoadDirective label  ]
]

{ #category : 'visiting' }
MetacelloLoadTarget >> visitPreLoadDirective: aMetacelloPreLoadDirective [

	aMetacelloPreLoadDirective spec preLoadDoIt ifNotNil: [ :symbol |
			MetacelloNotification signal: 'Executing: ' , aMetacelloPreLoadDirective label.

			MetacelloNotification 
				logTimeDuring: [
						symbol valueWithEnoughArguments: {
								(self baselineFor: aMetacelloPreLoadDirective).
								self.
								aMetacelloPreLoadDirective spec } ] 
				description: 'Done executing: ' , aMetacelloPreLoadDirective label  ]
]
