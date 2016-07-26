private
pharoDistributionRepository
	"Returns the correct pharo distribution url"

	^ 'http://smalltalkhub.com/mc/Pharo/MetaRepoFor' , self pharoCurrentMajorName , '/main'