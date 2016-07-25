# Metacello version format

Thanks to [Mozilla Toolkit version format](https://developer.mozilla.org/en/Toolkit_version_format) for inspiration.

##Version Format
A version string consists of one or more version parts, separated with dots or dashes.

A version part with a leading dot is numeric. A version part with a leading dash is string.

The rationale behind splitting a version part into a sequence of strings and numbers is that when comparing version parts, the numeric parts are compared as numbers, e.g. '1.0-pre.1' < '1.0-pre.10', while the strings are compared bytewise. See the next section for details on how versions are compared.

##Comparing versions

When two version strings are compared, their version parts are compared left to right. Empty parts are ignored.

If at some point a version part of one version string is greater than the corresponding version part of another version string, then the first version string is greater than the other one.

If a version string has extra parts and the common parts are equal, the shorter version string is less than the longer version string (1.0 is less than 1.0.0).

Otherwise, the version strings are equal. 

##Comparing version parts

Version parts are also compared left to right, A string-part that exists is always less-then a nonexisting string-part (1.6-a is less than 1.6).

Examples

```
1 == 1. < 1.0 == 1..--0
< 1.1-a < 1.1-aa < 1.1-ab < 1.1-b < 1.1-c
< 1.1-pre < 1.1-pre.0 
< 1.1-pre.1-a < 1.1-pre.1-aa < 1.1-pre.1-b < 1.1-pre.1
< 1.1-pre.2
< 1.1-pre.10
< 1.1 < 1.1.0 < 1.1.00
< 1.10
< 2.0
```