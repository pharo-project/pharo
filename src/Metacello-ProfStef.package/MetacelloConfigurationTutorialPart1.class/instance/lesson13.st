lessons
lesson13
	^Lesson
		title: 'Lesson 12 (DoIts)'
		lesson: 
'
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #baseline13:.
Smalltalk tools browser fullOnClass: MetacelloTutorialConfig selector: #version13:.

"For version 1.3 we are adding a platform specific package ''Example-Platform''. ''Example-Platform''
requires ''Example-Core''. On GemStone, Pharo and Squeak, a branch of the ''Example-Platform'' package 
will be loaded: ''Example-Platform.gemstone'', ''Example-Platform.pharo'', ''Example-Platform.squeak'' 
respectively will be loaded. 

Consequently we''ve updated the baselines with #baseline13: to reflect the structural changes and
#version13: reflects the package versions.

The platform-specific versions and branches are defined in the #for:do: block for the corresponding
platforms: #gemstone, #pharo, #squeak (in both methods)

The result of the following expression will depend on the platform upon which you are running:"

	(MetacelloTutorialConfig project version: ''1.3'') load.

"Note that when you execute the following expresson to load ''Example-Core'' that the correct
''Example-Platform'' is loaded as well:"

	(MetacelloTutorialConfig project version: ''1.3'') load: ''Example-Core''.

"If you look at the specification for ''Example-Core'' (in #baseline13:) you will note that 
''Example-Core'' #includes: ''Example-Platform''. The #includes: directive means that the package 
''Example-Platform'' should be loaded whenever the ''Example-Core'' package is loaded.

Also note when you evaluate the following expression that the ''Example-Platform'' package is loaded
before ''Example-Tests'' as if ''Example-Tests'' #requires: ''Example-Platform'':"

	(MetacelloTutorialConfig project version: ''1.3'') load: ''Example-Tests''.

"When you use the #includes: directive, you are not only specifying that the listed packages should
be loaded when the parent package is loaded, but that the #included: packages should be loaded
_before_ any packages that require the parent package."

ProfStef next.
'