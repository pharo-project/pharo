"
I am a project of a MetacelloResolution: the project at the version its resolver chose.

- projectSpec: a MetacelloProjectSpec at that version, whose project holds the version spec read from its baseline; its loads are the union of the groups every request asked, the default ones counted as their names.
- requests: the MetacelloProjectRequests for me: who asked (another resolved project, nil for the root) and the project spec they declared, with its groups and findings.
- installsBaseline: true when my resolver read my baseline without installing it; a load then installs my BaselineOf class once the projects are loaded.
"
Class {
	#name : 'MetacelloResolvedProject',
	#superclass : 'Object',
	#instVars : [
		'projectSpec',
		'requests',
		'installsBaseline'
	],
	#category : 'Metacello-Core-Resolution',
	#package : 'Metacello-Core',
	#tag : 'Resolution'
}

{ #category : 'instance creation' }
MetacelloResolvedProject class >> projectSpec: aProjectSpec [
	"aProjectSpec is at the chosen version, its project built from that version's baseline"

	^ self new setProjectSpec: aProjectSpec
]

{ #category : 'accessing' }
MetacelloResolvedProject >> beInstallingBaseline [

	installsBaseline := true
]

{ #category : 'accessing' }
MetacelloResolvedProject >> changeVersionTo: aProjectSpec [
	"Another version of the project was chosen: take aProjectSpec, keeping the groups asked so far and who asked them"

	| loads |
	loads := projectSpec loads.
	projectSpec := aProjectSpec copy.
	projectSpec loads: loads
]

{ #category : 'accessing' }
MetacelloResolvedProject >> className [

	^ projectSpec className
]

{ #category : 'accessing' }
MetacelloResolvedProject >> installsBaseline [
	"Its BaselineOf class is not in the image at the chosen version (the resolver read it without installing it): a load installs it once the project is loaded, for the tools"

	^ installsBaseline == true
]

{ #category : 'accessing' }
MetacelloResolvedProject >> loadList [
	"The names to load from this project: every group asked"

	^ projectSpec loads
]

{ #category : 'accessing' }
MetacelloResolvedProject >> name [

	^ projectSpec name
]

{ #category : 'printing' }
MetacelloResolvedProject >> printOn: aStream [

	aStream
		nextPutAll: self name;
		nextPutAll: ' from '.
	projectSpec repositoryDescriptions do: [ :each | aStream nextPutAll: each ] separatedBy: [ aStream nextPutAll: ', ' ]
]

{ #category : 'accessing' }
MetacelloResolvedProject >> projectSpec [
	"The project at its chosen version, loading the groups every request asked"

	^ projectSpec
]

{ #category : 'requests' }
MetacelloResolvedProject >> requestedBy: aResolvedProjectOrNil spec: aProjectSpec [
	"Record that aResolvedProjectOrNil (nil for the root) asked for this project as aProjectSpec declares it, and answer the MetacelloProjectRequest. Groups add up: the project is loaded once, with every group asked, the default ones counted as their names"

	| request |
	request := MetacelloProjectRequest requester: aResolvedProjectOrNil projectSpec: aProjectSpec.
	requests add: request.
	projectSpec loads:
		(projectSpec loads , (request loads asArray ifEmpty: [ self version defaultPackageNames asArray ])) asSet asArray.
	^ request
]

{ #category : 'accessing' }
MetacelloResolvedProject >> requests [
	"Requester name (nil for the root) -> the groups it asked, empty for its default ones"

	^ requests
]

{ #category : 'accessing' }
MetacelloResolvedProject >> setProjectSpec: aProjectSpec [

	projectSpec := aProjectSpec copy.
	projectSpec loads: #(  ).
	requests := OrderedCollection new
]

{ #category : 'accessing' }
MetacelloResolvedProject >> version [
	"The version spec read from the baseline at the chosen version"

	^ projectSpec version
]
