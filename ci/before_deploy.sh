# This script takes care of building your crate and packaging it for release

set -ex

main() {
    local src=$(pwd) \
          stage=

    case $TRAVIS_OS_NAME in
        linux)
            stage=$(mktemp -d)
            ;;
        windows)
            stage=$(mktemp -d)
            ;;
        osx)
            stage=$(mktemp -d -t tmp)
            ;;
    esac

    test -f Cargo.lock || cargo generate-lockfile

    # TODO Update this to build the artifacts that matter to you
    cargo rustc --bin railplot --target $TARGET --release -- -C lto

    # TODO Update this to package the right artifacts
    cp target/$TARGET/release/railplot $stage/

    for f in examples/bundled/*railml; do 
        cp $f $stage/example-$(basename -- "$f").xml
    done

    cd $stage
    case $TRAVIS_OS_NAME in
        linux)
            tar czf $src/$CRATE_NAME-$TRAVIS_TAG-$BINNAME.tar.gz *
            ;;
        osx)
            tar czf $src/$CRATE_NAME-$TRAVIS_TAG-$BINNAME.tar.gz *
            ;;
        windows)
            7z a $src/$CRATE_NAME-$TRAVIS_TAG-$BINNAME.zip *
            ;;
    esac
    cd $src

    rm -rf $stage
}

main
