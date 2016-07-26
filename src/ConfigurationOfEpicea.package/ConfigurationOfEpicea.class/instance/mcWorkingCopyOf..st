scripting
mcWorkingCopyOf: aPackageName

	^ MCWorkingCopy allManagers detect: [ :each | each package name = aPackageName ]