MetacelloSemanticVersionNumber conforms to version 2.0.0-rc.1 of  [Semantic Versioning 2.0.0-rc.1](http://semver.org/)

The most important thing that you need to know is that: 
**A normal version number MUST take the form X.Y.Z where X, Y, and Z are non-negative integers.**

**Semantic Versioning Specification** extracted from [Semantic versioning 2.0.0-rc.1](https://github.com/mojombo/semver/blob/3c7f2e8df747ea0ca15208fdfc90e3275240184f/semver.md):

Semantic Versioning Specification (SemVer)
------------------------------------------

The key words "MUST", "MUST NOT", "REQUIRED", "SHALL", "SHALL NOT", "SHOULD",
"SHOULD NOT", "RECOMMENDED", "MAY", and "OPTIONAL" in this document are to be
interpreted as described in RFC 2119.

1. Software using Semantic Versioning MUST declare a public API. This API
could be declared in the code itself or exist strictly in documentation.
However it is done, it should be precise and comprehensive.

1. A normal version number MUST take the form X.Y.Z where X, Y, and Z are
non-negative integers. X is the major version, Y is the minor version, and Z
is the patch version. Each element MUST increase numerically by increments of
one. For instance: 1.9.0 -> 1.10.0 -> 1.11.0.

1. Once a versioned package has been released, the contents of that version
MUST NOT be modified. Any modifications must be released as a new version.

1. Major version zero (0.y.z) is for initial development. Anything may change
at any time. The public API should not be considered stable.

1. Version 1.0.0 defines the public API. The way in which the version number
is incremented after this release is dependent on this public API and how it
changes.

1. Patch version Z (x.y.Z | x > 0) MUST be incremented if only backwards
compatible bug fixes are introduced. A bug fix is defined as an internal
change that fixes incorrect behavior.

1. Minor version Y (x.Y.z | x > 0) MUST be incremented if new, backwards
compatible functionality is introduced to the public API. It MUST be
incremented if any public API functionality is marked as deprecated. It MAY be
incremented if substantial new functionality or improvements are introduced
within the private code. It MAY include patch level changes. Patch version
MUST be reset to 0 when minor version is incremented.

1. Major version X (X.y.z | X > 0) MUST be incremented if any backwards
incompatible changes are introduced to the public API. It MAY include minor
and patch level changes. Patch and minor version MUST be reset to 0 when major
version is incremented.

1. A pre-release version MAY be denoted by appending a hyphen and a series of
dot separated identifiers immediately following the patch version. Identifiers
MUST be comprised of only ASCII alphanumerics and dash [0-9A-Za-z-].
Pre-release versions satisfy but have a lower precedence than the associated
normal version. Examples: 1.0.0-alpha, 1.0.0-alpha.1, 1.0.0-0.3.7,
1.0.0-x.7.z.92.

1. A build version MAY be denoted by appending a plus sign and a series of dot
separated identifiers immediately following the patch version or pre-release
version. Identifiers MUST be comprised of only ASCII alphanumerics and dash
[0-9A-Za-z-]. Build versions satisfy and have a higher precedence than the
associated normal version. Examples: 1.0.0+build.1, 1.3.7+build.11.e0f985a.

1. Precedence MUST be calculated by separating the version into major, minor,
patch, pre-release, and build identifiers in that order. Major, minor, and
patch versions are always compared numerically. Pre-release and build version
precedence MUST be determined by comparing each dot separated identifier as
follows: identifiers consisting of only digits are compared numerically and
identifiers with letters or dashes are compared lexically in ASCII sort order.
Numeric identifiers always have lower precedence than non-numeric identifiers.
Example: 1.0.0-alpha < 1.0.0-alpha.1 < 1.0.0-beta.2 < 1.0.0-beta.11 <
1.0.0-rc.1 < 1.0.0-rc.1+build.1 < 1.0.0 < 1.0.0+0.3.7 < 1.3.7+build <
1.3.7+build.2.b8f12d7 < 1.3.7+build.11.e0f985a.

About
-----

The Semantic Versioning specification is authored by [Tom
Preston-Werner](http://tom.preston-werner.com), inventor of Gravatars and
cofounder of GitHub.

If you'd like to leave feedback, please [open an issue on
GitHub](https://github.com/mojombo/semver/issues).


License
-------

Creative Commons - CC BY 3.0
http://creativecommons.org/licenses/by/3.0/
