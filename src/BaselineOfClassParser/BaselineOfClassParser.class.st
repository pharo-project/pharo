Class {
	#name : #BaselineOfClassParser,
	#superclass : #Object,
	#category : #BaselineOfClassParser
}

{ #category : #baselines }
BaselineOfClassParser >> baseline: spec [
	<baseline>
	spec for: #'common' do: [
		spec 
			package: 'ClassParser';
			package: 'ClassParser-Tests' with: [spec requires: #(ClassParser) ];
			package: 'UndefinedClasses';
			package: 'UndefinedClasses-Tests' with: [spec requires: #(UndefinedClasses)].

		spec 
			group: 'Core' with: #('ClassParser' 'UndefinedClasses');
			group: 'Tests' with: #('ClassParser-Tests' 'UndefinedClasses-Tests');
			group: 'default' with: #('Core' 'Tests') ]
]
