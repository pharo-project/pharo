private
daPackageFor: aPackageName
	^ (((self buildPackageRelationGraphFor: aPackageName) packages)
			select: #isSeen) first