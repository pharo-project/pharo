I'm a private and abstract filesystem path, independent of the string representation used to describe paths on a specific filesystem. I provide methods for navigating the filesystem hierarchy and working with absolute and relative paths. I only refer to a concrete file or directory with regard to a specific filesystem. Normally users should not use me directly. 

API instance creation:

- #from: parses the supplied string using the default delimeter
- #from:delimiter: parses the supplied string using the supplied delimiter.
- #/ creates an absolute path from the supplied string
- #* creates a relative path from the supplied string

API path manipulation:

- #/ adds the supplied string to the receiver
