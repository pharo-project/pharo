repository creation
registerPackageNamed: packageName
  ^ (Smalltalk at: #'PackageInfo') registerPackageName: packageName