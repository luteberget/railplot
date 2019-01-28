case $TRAVIS_OS_NAME in
    linux)
        sudo apt update
        sudo apt install -y zlib1g-dev
        sudo apt install -y g++-multilib gcc-multilib
        ;;
esac

