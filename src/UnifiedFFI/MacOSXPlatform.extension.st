Extension { #name : 'MacOSXPlatform' }

{ #category : '*UnifiedFFI' }
MacOSXPlatform >> isTranslated [
	"Tells if the Pharo VM is translated by Rosetta.
	See https://developer.apple.com/documentation/apple-silicon/about-the-rosetta-translation-environment#3616845 for more details.
	If it's translated:
	$ sysctl sysctl.proc_translated
	sysctl.proc_translated: 1

	If not translated:
	$ sysctl sysctl.proc_translated
	sysctl.proc_translated: 0

	On non-ARM Macs:
	$ sysctl sysctl.proc_translated
	sysctl: unknown oid 'sysctl.proc_translated'
	"

	^ (LibC resultOfCommand: 'sysctl -n sysctl.proc_translated') trim = '1'
]

{ #category : '*UnifiedFFI' }
MacOSXPlatform >> processorArchitecture [

	^ self isTranslated
		ifTrue: [ 'arm64' ]
		ifFalse: [ super processorArchitecture ]
]
