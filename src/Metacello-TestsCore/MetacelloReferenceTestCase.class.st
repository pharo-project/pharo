Class {
	#name : 'MetacelloReferenceTestCase',
	#superclass : 'TestCase',
	#category : 'Metacello-TestsCore',
	#package : 'Metacello-TestsCore'
}

{ #category : 'testing' }
MetacelloReferenceTestCase >> conditionResult: resultString [ 
	"GemStone unconditionally prints Symbols with quotes, Squeak and Pharo do not"

	^((((((resultString 
		copyReplaceAll: '#preloadForAddOn' with: '#''preloadForAddOn''')
			copyReplaceAll: '#postloadForAddOn' with: '#''postloadForAddOn''')
				copyReplaceAll: '#baseline' with: '#''baseline''')
					copyReplaceAll: '#preloadForVersion' with: '#''preloadForVersion''')
						copyReplaceAll: '#postloadForVersion' with: '#''postloadForVersion''')
							copyReplaceAll: '#preloadForProject' with: '#''preloadForProject''')
								copyReplaceAll: '#postloadForProject' with: '#''postloadForProject'''
]

{ #category : 'testing' }
MetacelloReferenceTestCase >> expectedPrintString [
    ^ 'spec preLoadDoIt: #''preloadForVersion''.
spec postLoadDoIt: #''postloadForVersion''.
spec repositories: [
	spec
		repository: ''/opt/mc/repository'';
		repository: ''c:\pharo\myapp\repo\'';
		repository: ''dictionary://GlobalName'';
		repository: ''ftp://ftp.example.com/repo'';
		repository: ''http://www.example.com/ar'';
		repository: ''http://www.example.com/private'' username: ''foo'' password: ''bar'';
		repository: ''http://www.example.com/rr'' ].
spec 
	baseline: ''UI Support'' with: [
		spec
			className: ''ConfigurationOfUI'';
			preLoadDoIt: #''preloadForProject'';
			postLoadDoIt: #''postloadForProject'';
			operator: #''~>'';
			loads: #(''UI-Core'' );
			repository: ''http://www.example.com/r'' ];
	baseline: ''UI Support'' overrides: [
		spec
			className: ''ConfigurationOfUINew'';
			operator: #''>='';
			loads: #(''UI-Core'' ''UI-Tests'' );
			repository: ''http://www.example.com/r'' username: ''foo'' password: ''bar'' ];
	baseline: ''UI Tests'' copyFrom: ''UI Support'' with: [
		spec
			className: ''ConfigurationOfUI'';
			operator: #''~>'';
			loads: #(''UI-Core'' ''UI-Tests'' );
			repository: ''http://www.example.com/r'' ];
	baseline: ''UI Support'' ;
	removeProject: ''UI Tests'';
	baseline: ''UI Multi'' with: [
		spec
			className: ''ConfigurationOfMulti'';
			repository: ''http://www.example.com/r'';
			repository: ''http://www.example.com/s'' ].
spec 
	package: ''Example-AddOn'' with: [
		spec 
			requires: #(''Example-Core'' );
			includes: #(''Example-UI'' );
			repositories: [
				spec
					repository: ''http://www.example.com/yar'';
					repository: ''http://www.example.com/yas'' ];
			preLoadDoIt: #''preloadForAddOn'';
			postLoadDoIt: #''postloadForAddOn''. ];
	package: ''Example-AddOn'' overrides: [
		spec 
			requires: #(''Example-Core'' ''UI Support'' );
			includes: #(''Example-UI'' );
			repository: ''http://www.example.com/or'' username: ''foo'' password: ''bar'';
			preLoadDoIt: #''preloadForAddOn'';
			postLoadDoIt: #''postloadForAddOn''. ];
	package: ''Example-Core'';
	package: ''Example-Tests'' with: [
		spec requires: #(''Example-Core'' ). ];
	package: ''Example-TestsUI'' with: [
		spec requires: #(''Example-UI'' ). ];
	package: ''Example-UI'' with: [
		spec requires: #(''Example-AddOn'' ). ];
	removePackage: ''Example-Tests''.
spec 
	group: ''default'' with: #(''Example-Core'' ''Example-AddOn'' );
	group: ''default'' overrides: #(''Example-Core'' ''Example-Tests'' );
	removeGroup: ''default''.'
]

{ #category : 'testing' }
MetacelloReferenceTestCase >> testReferenceConfig [

	| spec map string expected |
	spec := MetacelloReferenceBaseline project version spec.
	map := spec packages map.
	string := self conditionResult: (map at: 'Example-AddOn') printString.
	expected := 'spec 
	name: ''Example-AddOn'';
	requires: #(''Example-Core'' ''UI Support'' );
	includes: #(''Example-UI'' );
	repository: ''http://www.example.com/or'' username: ''foo'' password: ''bar'';
	preLoadDoIt: #''preloadForAddOn'';
	postLoadDoIt: #''postloadForAddOn''.'.
	self assert: string equals: expected.

	string := (map at: 'Example-Core') printString.
	self assert: string equals: 'spec name: ''Example-Core'''.

	string := (map at: 'Example-TestsUI') printString.
	self assert: string equals: 'spec 
	name: ''Example-TestsUI'';
	requires: #(''Example-UI'' ).'.

	string := (map at: 'UI Support') printString.
	self assert: string equals: 'spec 
	name: ''UI Support'';
	projectReference: [
		spec
			className: ''ConfigurationOfUINew'';
			operator: #''>='';
			loads: #(''UI-Core'' ''UI-Tests'' );
			repository: ''http://www.example.com/r'' username: ''foo'' password: ''bar'' ].'
]

{ #category : 'testing' }
MetacelloReferenceTestCase >> testReferenceConfigPrintString [

	| version string expected x y |
	version := MetacelloReferenceBaseline project version.
	string := self conditionResult: version spec printString.
	expected := self expectedPrintString.
	1 to: string size do: [ :i |
			(i > expected size or: [ (string at: i) ~= (expected at: i) ]) ifTrue: [
					x := string copyFrom: (i - 25 max: 1) to: (i + 25 min: string size).
					y := expected copyFrom: ((i - 25 max: 1) min: expected size) to: (i + 25 min: expected size).
					Array with: x with: y "halt" ] ].
	self assert: string equals: expected
]
