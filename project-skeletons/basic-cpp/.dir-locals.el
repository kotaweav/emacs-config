((nil . ((eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root)
                            "make -C build -j$(nproc)"
                            projectile-compilation-cmd-map)
                   (puthash (projectile-project-root)
                            "make run -C build"
                            projectile-run-cmd-map))))))
