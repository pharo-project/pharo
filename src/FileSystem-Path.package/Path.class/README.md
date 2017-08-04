I'm a private and abstract filesystem path, independent of the string representation used to describe paths on a specific filesystem. I provide methods for navigating the filesystem hierarchy and working with absolute and relative paths. I only refer to a concrete file or directory with regard to a specific filesystem. Normally users should not use me directly. 

Path independent representation of delimiter is defined by DiskFileSystem current delimiter.

API instance creation:
#* and #/ are mnemonic to . and /
whose arguments should  be string file- or directory names, not fragments of Unix path notation intended to be parsed.

#/ and #* provide a mini-DSL for building up paths, while
#readFrom:delimiter: parses path strings.

Note that (Path with: 'parent/child/') isRelative returns true
because it creates to a relative path to a file/directory called
'parent/child'. In bash you'd escape the slashes like this: parent\/child\/

similarly 
(Path with: '/parent/child/') isRelative returns true
That's a relative path to '/parent/child'. In bash: /\parent\/child\/

(Path with: '') isRelative returns false

Because this is an absolute path to the root of the file system. Absolute paths
have an empty first element. If you consider $/ the separator,
'/usr/local/bin' has an empty first element.

