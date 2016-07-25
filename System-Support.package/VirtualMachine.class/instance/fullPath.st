accessing
fullPath
	"Return the full vm path as a string"
	"Ex: '/Applications/Squeak/Squeak 4.2.5beta1U.app/Contents/MacOS/Squeak VM Opt'"
	"SmalltalkImage current vmFullPath"

	^self getSystemAttribute: 0