copying
postCopy
    super postCopy.
    repositories := repositories copy.
    packages := packages copy	"leave reference to packages for upgrade purposes"