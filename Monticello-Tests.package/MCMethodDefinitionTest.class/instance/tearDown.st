running
tearDown
       self restoreMocks.
       (MCWorkingCopy forPackage: (MCPackage named: 'FooBarBaz')) unregister.
       self class compile: 'override ^ 1' classified: 'mocks'.
       self ownPackage modified: isModified.

       "FIXME: Unregister Monticellomocks if it got created implicitly.
       This avoids a nasty failure of MCChangeNotificationTest due to
       some inconsistency about whether package names are case sensitive
       or not. They're treated as case insensitive in some name lookups but not
       in others; most importantly PackageOrganizer default treats package
       names as being case sensitive. The package created here is Monticellomocks
       (lower case mocks) and an instance of PackageInfo; the package expected
       in MCChangeNotificationTest is MonticelloMocks and an instance of
       MCMockPackageInfo. Since *that* lookup is case insensitive it can
       find Monticellomocks instead of MonticelloMocks and fail."
       "PackageOrganizer default unregisterPackageNamed: 'MonticelloMocks'."