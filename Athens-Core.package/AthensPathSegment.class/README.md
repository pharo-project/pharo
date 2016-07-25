The path segment is path building block.
Path is formed from list of connected path segments. At building stage, there is no container nor separately defined "path" object, just a linked list of segments.
Later the segments are accumulated in backend-specific path object, and the way how it is organized may vary.

My (sub)instances usually are not created directly by user, but instead by instance
of AthensPathBuilder.
Therefore, all my subclasses are considered private and implementation detail.