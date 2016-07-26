private
bootstrapMetacelloFrom: repositoryUrl
  "Corresponds to version 1.0.0-beta.32.6"

  "KEEP MetacelloConfigTemplate class>>ensureMetacelloBaseConfiguration in synch!"

  | platformPkg |
  self ensureGoferVersion: 'Gofer-Core-lr.115' repositoryUrl: repositoryUrl.
  #('Metacello-Core-dkh.678' 'Metacello-MC-dkh.674')
    do: [ :pkg | self bootstrapPackage: pkg from: repositoryUrl ].
  platformPkg := Smalltalk
    at: #'SystemVersion'
    ifPresent: [ :cl | 
      | versionString |
      versionString := cl current version.
      (versionString beginsWith: 'Squeak')
        ifTrue: [ 
          (versionString beginsWith: 'Squeak3')
            ifTrue: [ 'Metacello-Platform.squeak-dkh.5' ]
            ifFalse: [ 'Metacello-Platform.squeak-dkh.22' ] ]
        ifFalse: [ 
          (versionString beginsWith: 'Pharo')
            ifTrue: [ 
              self bootstrapPackage: 'Metacello-PharoCommonPlatform-dkh.2' from: repositoryUrl.
              (versionString beginsWith: 'Pharo2')
                ifTrue: [ 'Metacello-Platform.pharo20-dkh.33' ]
                ifFalse: [ 'Metacello-Platform.pharo-dkh.34' ] ] ] ].
  self bootstrapPackage: platformPkg from: repositoryUrl