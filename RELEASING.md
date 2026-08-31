# Releasing vcs-ignore

Releases use an annotated `vVERSION` tag. The tag workflow builds the Cabal
source distribution and executable archives for Linux, macOS, and Windows,
then creates a draft GitHub release.

## Prepare

1. Set the version in `package.yaml` and regenerate `vcs-ignore.cabal`.
2. Replace the changelog's development marker with the release date.
3. Merge the preparation pull request and wait for CI on `master`.
4. Confirm that `cabal check`, tests, Haddock, and `cabal sdist` pass.

## Build the draft release

Create and push the release tag from the tested `master` commit:

```console
git switch master
git pull --ff-only
git tag -a vVERSION -m "Release vVERSION"
git push origin vVERSION
```

The release workflow verifies that the tag matches the package version. After
all artifacts build successfully, it creates a draft GitHub release containing
the Hackage-ready source distribution, platform binaries, and checksums.

## Publish

1. Download the source distribution from the draft GitHub release.
2. Upload it as a Hackage candidate and inspect the generated package page:

   ```console
   cabal upload vcs-ignore-VERSION.tar.gz
   ```

3. Publish the same archive after the candidate is verified:

   ```console
   cabal upload --publish vcs-ignore-VERSION.tar.gz
   ```

4. Review the generated release notes and publish the GitHub draft.
5. Verify the public Hackage documentation and the downloadable binaries.

Never move or recreate a published release tag. Prepare a new package version
when a released artifact needs correction.
