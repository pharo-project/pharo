: Gofer, a person who runs errands. Origin 1960s: from go for, i.e. go and fetch.
: ''The New Oxford American Dictionary''

! Synopsis

Gofer is a small tool on top of Monticello that loads, updates, merges, diffs, reverts, commits, recompiles and unloads groups of Monticello packages. Contrary to existing tools Gofer makes sure that these operations are performed as clean as possible:

- Gofer treats packages from one or more repository in one operation.
- Gofer works with fixed versions or tries to find the "latest" version using a given package name.
- Gofer automatically assigns repositories to all packages, so that the other tools are ready to be used on individual packages.
- Gofer makes sure that there is only one repository instance registered for a single physical location.
- Gofer works with Monticello dependencies and uniformly treats them like the primary package.
- Gofer prefers to work with faster repositories if there is a choice.
- Gofer cleans up after Monticello, no empty class categories and no empty method protocols are to be expected.
- Gofer supports operations to sync remote and local repositories with each other.

! Installation

Gofer is included with the latest Pharo and GemStone distributions. To update to the latest version you can use Gofer itself:

== Gofer upgrade

In case you are missing Gofer in your image, grab it from *http://source.lukas-renggli.ch/gofer.html*.

! Description

Gofer is very simple by design, the basic useage scenario is always the same and consists of three steps:

# You specify one or more Monticello repository URLs. You can do this using the methods ==url:==, ==url:username:password:== (HTTP, FTP), ==directory:==, or ==repository:== if you need full control. You might also use the convenience methods like ==squeaksource:==, ==wiresong:==, or ==gemsource:== for well known repositories. Additionally the following settings are available:
#- Gofer implicitly declares the local package cache as a repository. To disable the local package cache use the method ==disablePackageCache==, to re-enable use ==enablePackageCache==.
#- Gofer throws an error if a repository is not reachable. To silently ignore repository erros use the message ==disableRepositoryErrors==, to re-enable use ==enableRepositoryErrors==.
# You specify one or more Monticello packages you want to work with, by adding them to the Gofer instance. Use ==version:== to add a specific version, or use ==package:== to add the "latest" version in the given repository. Furthermore there is ==package:constraint:== that allows you to further constraint the version to be loaded in a block passed in as the second argument.
# You specify one or more actions to be performed on the specified packages:

| ==load==	| Load the specified packages.
| ==update==	| Update the specified packages.
| ==merge==	| Merge the specified packages into their working copies.
| ==localChanges==	| Answer the changes between the base version and the working copy.
| ==browseLocalChanges==	| Browse the changes between the base version and the working copy.
| ==remoteChanges==	| Answer the changes between the working copy and the remote changes.
| ==browseRemoteChanges==	| Browse the changes between the working copy and the remote changes.
| ==cleanup==	| Cleans the specified packages.
| ==commit==	| Commit the modified specified packages.
| ==commit:==	| Commit the modified specified packages with the given commit message.
| ==revert==	| Revert the specified packages to the currently loaded version.
| ==recompile==	| Recompile the specified packages.
| ==reinitialize==	| Call the class side initializers on the specified packages.
| ==unload==	| Unload the specified packages.
| ==fetch==     | Download versions from remote repositories into the local cache.
| ==push==      | Upload local versions from local cache into remote repositories.

! Example

To use Gofer to update to exact versions of the Kom Server, the 'latest' code of Seaside 2.8 and the 'latest' code of the Scriptaculous package that is committed by the author with the initials 'lr' one could evaluate:

== Gofer new
==     squeaksource: 'KomHttpServer';
==     version: 'DynamicBindings-gc.7';
==     version: 'KomServices-gc.19';
==     version: 'KomHttpServer-gc.32';
==     update.
== Gofer new
==     squeaksource: 'Seaside';
==     package: 'Seaside2.8a';
==     package: 'Scriptaculous' constraint: [ :version | version author = 'lr' ];
==     load