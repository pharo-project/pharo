"
Turn the version into a release. It therefore appears as #stable afterward"".

"
Class {
	#name : #VSReleaseDevelopmentVersionCommand,
	#superclass : #VSDevCycleCommand,
	#category : #Versionner-Core-Commands
}

{ #category : #actions }
VSReleaseDevelopmentVersionCommand >> baselineFor: aVersion [
	"Return the baseline on which this version relies on"
	| baselineVersionString |
	
	baselineVersionString := aVersion importedVersions at: 1.
	^ aVersion project version: baselineVersionString
]

{ #category : #execute }
VSReleaseDevelopmentVersionCommand >> execute [
	|  devVersion releaseDialog |

	[ devVersion := workflow developmentVersion ]
		on: MetacelloVersionDoesNotExistError
		do: [ UIManager default inform: 'Cannot find a development version for this project!'.
			^ self ].
	self ensureDevelopmentVersionIsBaseline.
	(releaseDialog := VersionnerReleasePanel openForVersion: devVersion)
		okAction: [ 
			self 
				release:  releaseDialog model releaseNumberText
				andSolveSymbolic: releaseDialog model isSymbolicVersionResolutionChecked ]
]

{ #category : #actions }
VSReleaseDevelopmentVersionCommand >> mcVersionFor: aVersionString [
	| config |
	
	config := workflow project configurationClass.
	^ config project version: aVersionString
]

{ #category : #actions }
VSReleaseDevelopmentVersionCommand >> projectReference: projectName configurationFrom: aMCVersion [ 
	| fullProjectSpec |

	fullProjectSpec := (aMCVersion spec packageNamed: projectName) projectReference.
	^ fullProjectSpec projectClassProject
]

{ #category : #actions }
VSReleaseDevelopmentVersionCommand >> release: releaseVersionString andSolveSymbolic: needsSymbolicVersionResolution [
	releaseVersionString ifNil: [	
		UIManager default inform: 'Release aborted! Specify a valid release version string.'.
		^ self ].

	[ workflow releaseDevelopment: releaseVersionString.
	  needsSymbolicVersionResolution
		ifTrue: [ self resolveSymbolicVersionsIn: (self mcVersionFor: releaseVersionString) ] ]
	on: MetacelloValidationFailure 
	do: [ :error | (VSConfigurationValidationIssuesDisplayCommand target: error for: self) execute]
]

{ #category : #actions }
VSReleaseDevelopmentVersionCommand >> resolveSymbolicVersionsIn: aVersion [
	| metacelloToolBox | 
	
	metacelloToolBox := MetacelloToolBox new.
	metacelloToolBox project: aVersion project.
	
	metacelloToolBox
		modifyVersionMethodForVersion: aVersion versionString
		versionSpecsDo: [  :attribute :versionSpec |
			versionSpec 
				specListProjectDo: [ :spec | | projectRef projectRefConfiguration resolvedVersion |
					projectRef := spec projectReference.
					projectRefConfiguration := self 
						projectReference: projectRef name
						configurationFrom: (self baselineFor: aVersion).
					resolvedVersion := (projectRefConfiguration version: projectRef versionString) versionNumber.
					projectRef versionString: resolvedVersion ]
				packageDo: [ :spec ]
				groupDo: [ :spec ].
			true ].
	metacelloToolBox commitMethod
]

{ #category : #accessing }
VSReleaseDevelopmentVersionCommand >> title [
	^ 'release from development version'
]
