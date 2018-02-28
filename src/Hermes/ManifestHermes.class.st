"
Hermes is a tool to binary serialize/deserialize behavior.
It uses the basic elements in the bootstraped image, so it can be used to load code in a really small image.

Basically it contains the packages: 

   - Hermes: Core classes and behavior to load the code in the image. This is loaded in the default image. This package is loaded by the bootstrap process, so it should be small.
   - Hermes-Exporter: It has all the behavior to export the classes, traits and methods. This is not loaded in the default image, as  it is only used to export the code. This decition minimizes the footprint of hermes in the bootstrapped image.
   - Hermes-Extensions: This package extends the behavior of the core package. This allows to load extra behavior after the bootstrap image is generated. This is loaded by Hermes it self. 

To Load code check: HEInstaller and HermesCommandLineHandler. 
To Export code check the comments in the Hermes-Exporter package.
"
Class {
	#name : #ManifestHermes,
	#superclass : #PackageManifest,
	#category : #Hermes
}

{ #category : #'meta-data - dependency analyser' }
ManifestHermes class >> manuallyResolvedDependencies [
	^ #(#'RPackage-Core' #Files #'Collections-Abstract')
]
