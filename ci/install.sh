case $TRAVIS_OS_NAME in
    linux)
        sudo apt install -y zlib1g-dev
        sudo apt install -y libc6-dev-i386
        ;;
esac

