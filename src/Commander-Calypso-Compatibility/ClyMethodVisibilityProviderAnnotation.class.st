"
Deprecated: the method visibility providers are marked with the <calypsoMethodVisibilityProvider: aPriority> pragma (ClyMethodVisibilityGroupsResult class>>#methodVisibilityProvider). I am kept, in the deprecated Commander-Calypso-Compatibility package, for the projects which still use me, and I will be removed in Pharo 16. doc/Calypso/CommandAnnotationsMigration.md of the Pharo repository explains the migration.

I am special annotation which marks classes that they can provide special hierarchy to represent method visibility classes.
Users do not need to lookup my instances. They can simple use following method which will find appropriate class and create the hierarchy:

	ClyMethodVisibilityProvider defaultHierarchy

In simple case it is just inverse ClySubclassHierarchy. 
So when you expand inherited classes (first item in third browser pane) you will see selected class on top of list and then all superclasses.

But when class uses traits then traits are also shown in the hierarchy in special inheritance order. 
It is available only when trait plugin is loaded to the system. 
Plugin brings new type of hierarchy ClyTraitUserHierarchy which overrides default method visibility hierarchy by annotation.
During annotations lookup the first registered annotation is used. It has lagest priority. 
So ClyTraitUserHierarchy is just annotated with high priority value 100 which overrides default hierarchy annotation.

I expect that annotated classes will implement following class side method 

- hierarchyForMethodVisibility

Look at ClyMethodVisibilityProvider references for details
"
Class {
	#name : 'ClyMethodVisibilityProviderAnnotation',
	#superclass : 'ClassAnnotation',
	#category : 'Commander-Calypso-Compatibility-Annotations',
	#package : 'Commander-Calypso-Compatibility',
	#tag : 'Annotations'
}

{ #category : 'default hierarchy' }
ClyMethodVisibilityProviderAnnotation class >> defaultHierarchy [

	self registeredInstancesDo: [ :each | ^each createHierarchy ].

	self error: 'No hierarchy provider is found to represent method visibility hierarchy'
]

{ #category : 'hierarchy creation' }
ClyMethodVisibilityProviderAnnotation >> createHierarchy [
	^annotatedClass hierarchyForMethodVisibility
]
