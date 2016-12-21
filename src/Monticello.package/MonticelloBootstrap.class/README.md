This class contains the required behavior to bootstrap monticello in a new image. A new image has no monticello meta-data, working copies, or whatsoever. Then, Monticello (this package) is initially loaded using an ST file in chunk format (see the CodeImporter package). Once Monticello is installed, this class is required to:

 - recreate the corresponding MC working copies for each package in the system
 - reload all methods, recreate their source code (and thus create the .changes file)

I can be created from a directory using the #inDirectory: class message. e.g.,

  MonticelloBootstrap inDirectory: 'test'.

Then, the method #loadPackageVersion: will load a specific version, and #loadBootstrapPackages will load the packages in the correct order for the corresponding version of the bootstrap.

The convenience method #bootstrapDefault will create an instance with the default parameters for bootstrap and load all bootstrap packages.

  MonticelloBootstrap bootstrapDefault