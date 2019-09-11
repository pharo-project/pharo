"
Native-Browser is a package adding to the system the possibility of opening a directory in the native browser of the OS. 

Currently working on:
- OSX (32/64)
- Linux (32/64)
- Windows (32/64)

You can use it like this:

```
FileSystem workingDirectory openInNativeBrowser.

OSPlatform current openNativeBrowserOn: FileLocator home.
```
"
Class {
	#name : #ManifestNativeBrowser,
	#superclass : #PackageManifest,
	#category : #'Native-Browser-Manifest'
}
