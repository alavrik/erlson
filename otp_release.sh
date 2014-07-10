#!/bin/sh

OTP_VERSION=`erl -noshell -eval 'io:format("~s", [erlang:system_info(otp_release)]), halt().'`

case "$OTP_VERSION" in
        R13*)
                echo "r13"
                ;;
        R14*)
                echo "r14"
                ;;
        R15*)
                echo "r15"
                ;;
        R16*)
                echo "r16"
                ;;
        17*)
                echo "r17"
                ;;
        *)
                echo "error: unknown Erlang/OTP version: $OTP_VERSION" >&2
                exit 1
esac

