module TwistlockProvisioner.Configuration where
import Prelude hiding (FilePath)
import Filesystem.Path

data Configuration = Configuration {
	containerTemplateDir :: FilePath
}
