"
A SettingPackageFilter is a SettingFilter used to select settings from a given list of packages: a setting is kept only if the package in which it is defined is present in the given package list

Instance Variables
	packages:		<Collection>

packages
	- The collection of PackageInfo the filter is currently using
"
Class {
	#name : #SettingPackageFilter,
	#superclass : #SettingFilter,
	#instVars : [
		'packages'
	],
	#category : 'System-Settings-Filter'
}

{ #category : #filtering }
SettingPackageFilter >> keepSettingTreeNode: aSettingNode [ 
	| rp |
	^ (rp := aSettingNode receiverPackage) notNil and: [self packages includes: rp]
]

{ #category : #accessing }
SettingPackageFilter >> packages [
	"Answer the value of packages"

	^ packages
]

{ #category : #accessing }
SettingPackageFilter >> packages: aCollectionOfPackageInfo [
	"Set the value of packages"

	packages := aCollectionOfPackageInfo

]
