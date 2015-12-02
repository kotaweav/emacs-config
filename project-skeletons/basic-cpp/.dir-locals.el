((nil . ((eval . (progn
                   (require 'projectile)
                   (puthash (projectile-project-root)
                            "make -C build -j$(nproc)"
                            projectile-compilation-cmd-map))))))
