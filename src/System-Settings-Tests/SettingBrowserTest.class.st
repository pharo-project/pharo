"
A SettingBrowserTest is a test class for testing the behavior of SettingBrowser
"
Class {
	#name : 'SettingBrowserTest',
	#superclass : 'TestCase',
	#instVars : [
		'settingBrowser'
	],
	#category : 'System-Settings-Tests',
	#package : 'System-Settings-Tests'
}

{ #category : 'tests' }
SettingBrowserTest >> testOpening [
	"Non-Regression Test related to https://pharo.manuscript.com/f/cases/16138"
	settingBrowser := SettingBrowser open.
	settingBrowser close
]

{ #category : 'tests' }
SettingBrowserTest >> testOpeningOnPackage [

	settingBrowser := SettingBrowser new
		                  changePackageSet: { (self packageOrganizer packageNamed: 'System-Settings-Core') };
		                  open.
	settingBrowser close
]
