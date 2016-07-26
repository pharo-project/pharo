running
setUp

       "FIXME: Unregister Monticellomocks if it got created in another test
       (for example MCMethodDefinitionTest may create it implicitly).
       This avoids a nasty failure of MCChangeNotificationTest due to
       some inconsistency about the package info registered with it.
       If Monticellomocks was created earlier it will contain a 'regular'
       PackageInfo instance but the test requires it to be an MCMockPackageInfo"
"       MCWorkingCopy registry
               removeKey: (MCPackage new name: 'MonticelloMocks')
               ifAbsent:[].
       PackageOrganizer default unregisterPackageNamed: 'MonticelloMocks'."

       workingCopy := MCWorkingCopy forPackage: self mockPackage.