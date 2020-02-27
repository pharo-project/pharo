"
Native-Browser is a package adding to the system the possibility of opening a directory in the native OS file browser 

Currently working on:
- OSX (32/64)
- Linux (32/64)
- Windows (32/64)

You can use it like this:

```
FileSystem workingDirectory openInOSFileBrowser.

NativeBrowserOpenVisitor openOn: FileLocator home.
```
"
Class {
	#name : #ManifestNativeBrowser,
	#superclass : #PackageManifest,
	#category : #'Native-Browser-Manifest'
}
