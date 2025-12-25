import Development.Shake
import Development.Shake.Command
import Development.Shake.FilePath
import Development.Shake.Util

main :: IO ()
main = shakeArgs shakeOptions{shakeFiles="out"} $ do
    want ["out/server", "out/index.html", "out/new-adventures.js", "out/api.js"]

    phony "clean" $ do
        putInfo "Cleaning files in out"
        removeFilesAfter "out" ["//*"]

    "out/server" %> \_out -> do
        alwaysRerun
        -- writes to out/server without argument
        -- because executable name is “server”
        cmd_ "cabal install --installdir out --overwrite-policy always"

    "out/index.html" %> \out -> do
        let html = "src/client/new-adventures.html"
        need [html]
        copyFile' html out

    "out/new-adventures.js" %> \_out -> do
        files <- getDirectoryFiles "" ["src/client/*.ts", "src/client/*.js"]
        need files
        cmd_ "tsc --noEmit"
        cmd_ "bun build" files "--outdir out"

    "out/api.js" %> \out -> do
        need ["out/server"]
        cmd_ "out/server generate-api-javascript" out
