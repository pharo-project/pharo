pharo 2.0 compat
registerPackageNamed: packageName
  "PackageInfo deprecated in Pharo3.0"

  ^RPackageSet named: packageName
