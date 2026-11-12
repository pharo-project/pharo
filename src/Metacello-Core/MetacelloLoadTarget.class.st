"
I am the load target: I visit the load directives (fetched before) and load them through the engine's loader, atomically or one by one as the directives say, running their pre and post load do-its.

load: is the whole step: the directives, then the BaselineOf classes the resolver read without installing them, from the versions chosen, then the registry: every project of the resolution registered as loaded, with its version and groups.
"
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

{ #category : 'loading' }
MetacelloLoadTarget >> installResolvedBaselines [
	"Once the projects are loaded, install the BaselineOf classes the resolver read without installing, each from the version chosen"

	engine resolution projects do: [ :project |
		project installsBaseline ifTrue: [ engine doLoadProjectSpecFrom: project projectSpec projectPackage ] ]
]

{ #category : 'loading' }
MetacelloLoadTarget >> load: aDirective [
	"Load what aDirective says, install the BaselineOf classes the resolver read without installing, and register every project loaded"

	aDirective acceptVisitor: self.
	self installResolvedBaselines.
	self registerResolvedProjectsAsLoaded
]

{ #category : 'accessing' }
MetacelloLoadTarget >> loader [
	
	^ engine loader
]

{ #category : 'loading' }
MetacelloLoadTarget >> registerResolvedProjectsAsLoaded [
	"After a load, the registry says exactly what the image has: every project of the resolution, at its version, with the groups loaded from it, loaded in the image"

	engine resolution projects do: [ :project |
			| spec |
			spec := project projectSpec copy.
			MetacelloProjectRegistry
				registrationNamed: project className
				ifPresent: [ :existing |
						existing
							projectSpec: spec;
							loadedInImage: true ]
				ifAbsent: [
						spec asProjectRegistration
							loadedInImage: true;
							registerProject ] ]
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
