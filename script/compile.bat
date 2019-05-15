@echo off
if "%GRAALVM_HOME%"=="" ( 
    echo Please set GRAALVM_HOME
    exit /b
)
set %JAVA_HOME%=%GRAAL_VM_HOME%\bin
set %PATH%=%PATH%;%GRAAL_VM_HOME%\bin

set /P CLJ_KONDO_VERSION=< resources\CLJ_KONDO_VERSION
echo Building clj-kondo %CLJ_KONDO_VERSION%

call lein clean
if %errorlevel% neq 0 exit /b %errorlevel%

call lein uberjar
if %errorlevel% neq 0 exit /b %errorlevel%

Rem the --no-server option is not supported in GraalVM Windows.
call %GRAALVM_HOME%\bin\native-image.cmd ^
  -jar target/clj-kondo-%CLJ_KONDO_VERSION%-standalone.jar ^
  -H:Name=clj-kondo ^
  -H:+ReportExceptionStackTraces ^
  -J-Dclojure.spec.skip-macros=true ^
  -J-Dclojure.compiler.direct-linking=true ^
  "-H:IncludeResources=clj_kondo/impl/cache/built_in/.*" ^
  -H:ReflectionConfigurationFiles=reflection.json ^
  --initialize-at-build-time  ^
  -H:Log=registerResource: ^
  --verbose ^
  "-J-Xmx3g"
if %errorlevel% neq 0 exit /b %errorlevel%

echo Creating zip archive
jar -cMf clj-kondo-%CLJ_KONDO_VERSION%-windows-amd64.zip clj-kondo.exe
