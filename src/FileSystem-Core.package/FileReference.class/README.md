I combine a filesystem and path, which is sufficient to refer to a concrete file or directory. I provide methods for navigating my filesystem, performing filesystem operations and opening and closing files.  I am the primary mechanism for working with files and directories. 

| working |
working := FileSystem disk workingDirectory.
working files 

| disk |
disk := FileSystem disk.
disk root.                               	"a reference to the root directory"
disk working.                         	"a reference to the working directory"