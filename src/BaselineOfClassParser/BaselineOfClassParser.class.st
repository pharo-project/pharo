"
I describe how to load the class definition parser (`ClassParser`) and the undefined classes standing for missing superclasses (`UndefinedClasses`), with their tests.

## Examples

```
BaselineOfClassParser instanceDefault
```

## Design Rationale

- `ClassParser` and `UndefinedClasses` only need the packages of the bootstrap: my `Core` group declares no dependency on another project.
- My `Tests` group loads and runs in isolation, on the Metacello image (`Tests-ISO-ClassParser` stage of the `Jenkinsfile`). The tests therefore have to declare every project they use.
- `ClassParser-Tests` requires `Collections-Abstract-Tests` because it defines classes composed of the traits of the collection tests.
- Neither `ClassParser` nor its tests use `UndefinedClasses`: the dependency only goes from `UndefinedClasses-Tests` to `ClassParser`.
- `UndefinedClasses-Tests` requires `ClassParser` because it builds its classes from parsed definitions.

`BaselineOfIDE>>baseline:` loads my default group in the full image.

"
Class {
	#name : 'BaselineOfClassParser',
	#superclass : 'BaselineOf',
	#category : 'BaselineOfClassParser',
	#package : 'BaselineOfClassParser'
}

{ #category : 'instances' }
BaselineOfClassParser class >> instanceDefault [

	<sampleInstance>
	^ self new
]

{ #category : 'baselines' }
BaselineOfClassParser >> baseline: spec [

	<baseline>
	| repository |
	repository := self packageRepositoryURLForSpec: spec.
	spec for: #common do: [
		"The tests define classes using the traits of the collection tests"
		spec baseline: 'CollectionsTests' with: [
			spec
				repository: repository;
				className: 'BaselineOfCollections';
				loads: #( 'Collections-Abstract-Tests' ) ].

		spec
			package: 'ClassParser';
			package: 'ClassParser-Tests' with: [ spec requires: #( 'ClassParser' 'CollectionsTests' ) ];
			package: 'UndefinedClasses';
			package: 'UndefinedClasses-Tests' with: [ spec requires: #( 'UndefinedClasses' 'ClassParser' ) ].

		spec
			group: 'Core' with: #( 'ClassParser' 'UndefinedClasses' );
			group: 'Tests' with: #( 'ClassParser-Tests' 'UndefinedClasses-Tests' );
			group: 'default' with: #( 'Core' 'Tests' ) ]
]
